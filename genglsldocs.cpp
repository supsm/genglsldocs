#include <cctype>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

enum class comment_type { none, single, multi };
enum class token_type { identifier, number, string, block, preprocessor, other };

// skip all whitespace characters
// @tparam count  return number of skipped '\n' characters
template<bool count_newline = false>
std::conditional_t<count_newline, std::size_t, void> skip_ws(std::ifstream& fin)
{
	char c = fin.peek();
	std::size_t num;
	if constexpr (count_newline)
		{ num = 0; }
	while (fin)
	{
		bool ok = false;
		switch (fin.peek())
		{
		case '\n':
			if constexpr (count_newline)
				{ num++; }
			[[fallthrough]];
		case ' ': [[fallthrough]];
		case '\f': [[fallthrough]];
		case '\r': [[fallthrough]];
		case '\t': [[fallthrough]];
		case '\v':
			ok = true;
			break;
		}
		if (!ok) { break; }
		fin.ignore();
	}
	if constexpr (count_newline)
		{ return num; }
}

// read one comment (single or multi line)
// single line comments do not have whitespace removed
// @tparam no_skip_ws  do not skip whitespace at beginning. expects first character to be '/'
// @tparam put_back_ws  put back skipped whitespace after non-comment content. incompatible with no_skip_ws
// @return { type, content }
template<bool no_skip_ws = false, bool put_back_ws = false>
std::pair<comment_type, std::string> read_single_comment(std::ifstream& fin)
{
	static_assert(!(no_skip_ws && put_back_ws), "must skip whitespace (no_skip_ws == false) to put it back");
	
	std::streampos pos;
	if constexpr (put_back_ws)
	{
		pos = fin.tellg();
	}
	#define RESET_AND_RETURN() \
		if constexpr (put_back_ws) { fin.seekg(pos); } \
		return { comment_type::none, {} }
	
	if constexpr (!no_skip_ws)
	{
		skip_ws(fin);
	}
	
	if (!fin)
	{
		RESET_AND_RETURN();
	}
	
	// both multiline and single line comments start with '/'
	if (fin.peek() != '/')
	{
		RESET_AND_RETURN();
	}
	// consume /
	fin.ignore();
	// TODO: check if fin is still valid
	if (fin.peek() == '/') // single line comment
	{
		fin.ignore();
		std::string s;
		std::getline(fin, s);
		// TODO: read another if last character is backslash
		// (see read_processor)
		return { comment_type::single, s };
	}
	else if (fin.peek() == '*') // multi line comment
	{
		fin.ignore();
		std::string s;
		bool asterisk = false;
		while (fin)
		{
			char c = fin.get();
			if (asterisk)
			{
				if (c == '/') // end comment block
				{
					break;
				}
				// not end comment, add back asterisk
				s += c;
				asterisk = false;
			}
			if (c == '*')
			{
				asterisk = true;
				continue;
			}
			// don't add asterisk to string (may be part of end comment)
			s += c;
			if (c == '\n')
			{
				skip_ws(fin); // skip whitespace of next line
			}
		}
		return { comment_type::multi, s };
	}
	else // not a comment
	{
		fin.unget();
	}
	RESET_AND_RETURN();
	#undef RESET_AND_RETURN
}

// read multiple comments, either all available or just a single "block"
// (1 multiline or multiple single lines until double newline)
// will contain a trailing newline
// @tparam only_one_block  false to read all available comments, true to stop after reading a block
template<bool only_one_block = false>
std::string read_comment(std::ifstream& fin)
{
	std::string str;

	const auto add_all_but_last = [&str](const std::string& new_content)
	{
		if (new_content.size() <= 1)
		{
			return;
		}
		str.insert(str.end(), new_content.begin(), new_content.end() - 1);
	};

	bool first = true;
	comment_type prev_type = comment_type::none;
	while (fin)
	{
		const auto num_newlines = skip_ws<true>(fin);
		if constexpr (only_one_block)
		{
			// if there are 2 or more newlines between
			// comments, they are not within the same "block"
			if (!first && num_newlines > 1)
			{
				break;
			}
		}
		
		const auto& [type, content] = read_single_comment<true>(fin);
		if (type == comment_type::none)
		{
			break;
		}

		// multiline comments and multi followed by single
		// are parts of different blocks
		if (!first && (type == comment_type::multi || prev_type == comment_type::multi))
		{
			if constexpr (only_one_block)
			{
				break;
			}
			else
			{
				str += '\n';
			}
		}

		if (!content.empty())
		{
			// remove trailing \r if exists
			if (content.back() ==  '\r')
			{
				add_all_but_last(content);
			}
			else
			{
				str += content;
			}
			str += '\n';
		}

		prev_type = type;
		first = false;
	}
	return str;
}

std::string read_identifier(std::ifstream& fin)
{
	if (!fin)
	{
		return {};
	}
	
	skip_ws(fin);
	// valid character to start identifier: letter or underscore
	constexpr auto is_identifier_start = [](char c)
	{
		return (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
			c == '_');
	};
	// valid character in identifier: letter, digit, or underscore
	constexpr auto is_identifier = [is_identifier_start](char c)
	{
		return is_identifier_start(c) || ('0' <= c && c <= '9');
	};
	
	std::string s;
	if (is_identifier_start(fin.peek()))
	{
		s += fin.get();
	}
	else
	{
		return {};
	}
	while (fin)
	{
		if (is_identifier(fin.peek()))
		{
			s += fin.get();
		}
		else
		{
			break;
		}
	}
	return s;
}

// i suppose glsl doesn't have char but i already did this so oh well
// @return -1 for error or char value (0-256) otherwise
int read_char(std::ifstream& fin)
{
	skip_ws(fin);
	
	if (!fin)
	{
		return -1;
	}
	
	if (fin.peek() != '\'') // expect start char
	{
		return -1;
	}
	fin.ignore();
	if (fin.peek() == '\'') // expect something instead of immediate end char
	{
		return -1;
	}
	bool esc = false;
	if (fin.peek() == '\\') // skip an additional char if escape
	{
		fin.ignore();
		esc = true;
	}
	char c = fin.get();
	if (fin.peek() != '\'')
	{
		using namespace std::string_literals;
		// TODO: get file name
		throw std::runtime_error("Syntax error at position "s + std::to_string(fin.tellg()) + " in file ???:\n" +
			"char is more than 1 character");
	}
	if (esc)
	{
		switch (c)
		{
		case 'a':
			c = 0x07;
			break;
		case 'b':
			c = 0x08;
			break;
		case 'f':
			c = 0x0c;
			break;
		case 'n':
			c = 0x0a;
			break;
		case 'r':
			c = 0x0d;
			break;
		case 't':
			c = 0x09;
			break;
		case 'v':
			c = 0x0b;
			break;
		}
		// octal/hex and unicode escape sequences not supported
	}
	return static_cast<unsigned char>(c);
}

std::string read_string(std::ifstream& fin)
{
	skip_ws(fin);
	if (!fin)
	{
		return {};
	}
	
	if (fin.peek() != '\"') // expect to start string literal
	{
		return {};
	}
	fin.ignore();
	std::string contents;
	while (fin)
	{
		char c = fin.peek();
		switch (c)
		{
		case '\\': // escape character, skip extra char
			fin.ignore();
			break;
		case '\"': // TODO: end string
			contents += c;
			return contents;
		}
		contents += c;
		fin.ignore(); // consume char
	}
	// TODO: find file
	throw std::runtime_error("Syntax error in file ???: EOF reached with unterminated string");
}

std::string read_block(std::ifstream& fin)
{
	skip_ws(fin);
	if (fin.peek() != '{')
	{
		return {};
	}
	fin.ignore();
	
	std::size_t num_braces = 1;
	std::string contents = "{";
	while (fin && num_braces != 0)
	{
		char c = fin.peek();
		std::string curstr;
		switch (c)
		{
		case '/': // potential comment
			{
				auto [type, contents] = read_single_comment<false, true>(fin);
				switch (type)
				{
				case comment_type::none:
					break;
				case comment_type::multi:
					curstr += "/*" + contents + "*/";
					break;
				case comment_type::single:
					curstr += "//" + contents + '\n';
					break;
				}
				break;
			}
		case '\'': // char
			curstr = read_char(fin);
			break;
		case '\"': // string
			curstr = read_string(fin);
			break;
			
		case '{': // open brace
			num_braces++;
			break;
		case '}': // close brace
			num_braces--;
			break;
		}
		
		// something more than current char has been read
		if (!curstr.empty())
		{
			contents += curstr;
		}
		else
		{
			contents += c;
			fin.ignore();
		}
	}
	return contents;
}

// read preprocessor directive
// TODO: actually tokenize and parse #include/#define
std::string read_preprocessor(std::ifstream& fin)
{
	skip_ws(fin);
	if (fin.peek() != '#') // preprocessor directives begin with #
	{
		return {};
	}
	std::string str, line;
	bool another_line;
	do
	{
		if (!fin)
		{
			break;
		}
		skip_ws(fin); // skip whitespace at beginning of lines
		another_line = false;
		
		std::getline(fin, line);
		if (!line.empty() && line.back() == '\r') // deal with crlf (file is opened in binary mode)
		{
			line.pop_back();
		}
		if (!line.empty() && line.back() == '\\')
		{
			line.pop_back();
			another_line = true;
		}
		str += line;
		// read another line if last character is backslash
		// (whitespace after causes no escape)
		// TODO: line ending stuff
	} while (another_line);
	return str;
}

// read statement and tokenize
std::vector<std::pair<std::string, token_type>> read_statement(std::ifstream& fin)
{
	std::string str = read_preprocessor(fin);
	if (!str.empty())
	{
		return { { str, token_type::preprocessor } };
	}
	
	// parse into tokens
	std::vector<std::pair<std::string, token_type>> tokens;
	while (fin)
	{
		skip_ws(fin);
		std::string token = read_identifier(fin);
		token_type type = token_type::identifier;
		if (!token.empty()) // token is identifier, no need to check extra stuff
		{
			tokens.emplace_back(std::move(token), token_type::identifier);
			continue;
		}
		
		char c = fin.peek();
		// TODO: check for numbers (including literals)
		if (c == '\"')
		{
			token = read_string(fin);
			type = token_type::string;
		}
		else if (c == '\'')
		{
			token = read_char(fin);
			type = token_type::number;
		}
		else if (c == '{')
		{
			token = read_block(fin);
			tokens.emplace_back(std::move(token), token_type::block);
			break; // block ends a statement
		}
		if (!token.empty()) // token is number, string, char, or block
		{
			tokens.emplace_back(std::move(token), type);
			continue;
		}
		
		// no valid identifier, read until next space
		// TODO: this is probably not ideal, e.g. `!!i` (C hack to convert to bool) gives !! single token
		while (fin)
		{
			bool b = false;
			c = fin.peek();
			switch (c)
			{
				// possible multi-character tokens: add and continue
				// arithmetic (no pointers in glsl)
			case '+': [[fallthrough]];
			case '-': [[fallthrough]];
			case '*': [[fallthrough]];
			case '/': [[fallthrough]];
			case '%': [[fallthrough]];
				// logic/bitwise
			case '&': [[fallthrough]];
			case '|': [[fallthrough]];
			case '^': [[fallthrough]];
			case '!': [[fallthrough]];
				// compare/bitshift
			case '<': [[fallthrough]];
			case '>':
				token += c;
				fin.ignore();
				break;

				// whitespace: exit
			case ' ':  [[fallthrough]];
			case '\f': [[fallthrough]];
			case '\n': [[fallthrough]];
			case '\r': [[fallthrough]];
			case '\t': [[fallthrough]];
			case '\v':
				b = true;
				break;

				// other: add and exit
			default:
				token += c;
				fin.ignore();
				b = true;
				break;
			}

			if (b)
			{
				break;
			}
		}
		if (token.empty()) // probably fin is invalid
		{
			continue;
		}
		tokens.emplace_back(std::move(token), token_type::other);
		
		if (tokens.back().first == ";") // semicolon ends a statement
		{
			break;
		}
	}
	return tokens;
}

std::string read_variable(const std::vector<std::pair<std::string, token_type>>& tokens)
{
	// attributes... type name ("=" value) ";"
	// check if tokens represent a variable declaration
	
	// glorified non-throwing assert that works outside of debug
	#define CHECK_OR_RETURN(cond) if (!(cond)) { return {}; }
	
	// need at least 3 tokens (type, name, semicolon)
	CHECK_OR_RETURN(tokens.size() >= 3);
	// first 2 must be identifiers; type + name (type attributes are also identifiers)
	// assume no pointers/references/non-identifier types
	CHECK_OR_RETURN(tokens[0].second == token_type::identifier);
	CHECK_OR_RETURN(tokens[1].second == token_type::identifier);
	{
		// TODO: array declarations
		bool passed_assignment = false;
		for (std::size_t i = 2; i < tokens.size(); i++)
		{
			if (tokens[i].second == token_type::other && tokens[i].first == ";")
			{
				break;
			}
			if (tokens[i].second == token_type::other && tokens[i].first == "=")
			{
				// assume anything is a valid expression and only check last character
				CHECK_OR_RETURN(tokens.back().second == token_type::other && tokens.back().first == ";")
				break;
			}
			CHECK_OR_RETURN(tokens[i].second == token_type::identifier);
		}
	}
	
	#undef CHECK_OR_RETURN
	
	std::string doc_header = "## `";
	for (std::size_t i = 0; i < tokens.size() - 1; i++) // skip last ";" token
	{
		doc_header += tokens[i].first;
		doc_header += ' '; // TODO: trailing space
	}
	doc_header += "`\n";
	return doc_header;
}

// @return pair of header (function signature) and body
// TODO: do something with return type
std::pair<std::string, std::string> read_function(const std::vector<std::pair<std::string, token_type>>& tokens)
{
	// declarator "(" parameters ")" qualifiers... ";"/block
	// declarator = attributes... type name
	
	// glorified non-throwing assert that works outside of debug
	#define CHECK_OR_RETURN(cond) if (!(cond)) { return {}; }
	
	// check if tokens represent a function
	// need at least 5 tokens (return type, function name, open+close paren, block/semicolon)
	CHECK_OR_RETURN(tokens.size() >= 5);
	// first 2 must be identifiers; type + name (type attributes are also identifiers)
	// types are all identifiers because glsl has no references/funny pointers
	CHECK_OR_RETURN(tokens[0].second == token_type::identifier);
	CHECK_OR_RETURN(tokens[1].second == token_type::identifier);
	
	std::size_t initial_identifiers = 2; // num of identifiers up to '(': function attributes + return type + function name
	std::vector<std::pair<std::size_t, std::size_t>> params; // [start, end]
	std::vector<std::size_t> qualifiers;
	bool is_prototype;
	{
		bool passed_initial = false;
		std::size_t i;
		for (i = 2; i < tokens.size(); i++)
		{
			if (!passed_initial && tokens[i].second == token_type::identifier)
			{
				initial_identifiers++;
			}
			else if (tokens[i].second == token_type::other && tokens[i].first == "(")
			{
				passed_initial = true;
				bool any_params = false;
				std::size_t identifier_count = 0;
				// check params are valid
				// TODO: maybe this sort of pseudo-nested loop paradigm is not good
				for (i++; i < tokens.size(); i++)
				{
					if (tokens[i].second == token_type::identifier)
					{
						if (identifier_count == 0)
						{
							params.emplace_back(i, -1);
						}
						identifier_count++;
					}
					else if (tokens[i].second == token_type::other && tokens[i].first == ",")
					{
						// needs at least 1 identifier for param type
						// name not necessary
						CHECK_OR_RETURN(identifier_count >= 1);
						identifier_count = 0;
						any_params = true;
						params.back().second = i - 1; // i refers to ,
					}
					else
					{
						// only other token should be close parentheses to end param list
						CHECK_OR_RETURN(tokens[i].second == token_type::other && tokens[i].first == ")");
						break;
					}
				}
				// if there are any commas (2+ params), there should be an additional
				// param unaccounted for (no comma for last param)
				CHECK_OR_RETURN(!any_params || identifier_count >= 1);
				if (identifier_count >= 1)
				{
					// i is ')' character, last param ends before then
					params.back().second = i - 1;
				}
			
				for (i++; i < tokens.size(); i++)
				{
					// function qualifier
					if (tokens[i].second == token_type::identifier)
					{
						qualifiers.push_back(i);
					}
					else if (tokens[i].second == token_type::other && tokens[i].first == ";" ||
						tokens[i].second == token_type::block)
					{
						break;
					}
					else
					{
						return {};
					}
				}
				// should have broke when reaching semicolon (prototype) or block (definition)
				// if loop completed fully, i should be tokens.size()
				CHECK_OR_RETURN(i == tokens.size() - 1);
				
				if (tokens[i].second == token_type::block)
				{
					is_prototype = false;
				}
				else
				{
					is_prototype = true;
				}
				break;
			}
			else // not initial identifier or (
			{
				return {};
			}
		}
		// if loop exited without break, then everything was identifier
		// which shouldn't be possible due to how statements end
		// but this is here as a sanity check
		CHECK_OR_RETURN(i != tokens.size());
	}
	
	#undef CHECK_OR_RETURN
	
	
	// function name and param types (for overloading purposes)
	std::string function_doc_header = "## `" + tokens[initial_identifiers - 1].first + '(';
	if (params.empty())
	{
		function_doc_header += ")`";
	}
	else
	{
		for (const auto [start, end] : params)
		{
			for (auto i = start; i <= end; i++)
			{
				function_doc_header += tokens[i].first;
				function_doc_header += ' ';
			}
			// replace last ' ' with ','
			function_doc_header[function_doc_header.size() - 1] = ',';
			function_doc_header += ' ';
		}
		// replace last ", " with ")`"
		function_doc_header[function_doc_header.size() - 2] = ')';
		function_doc_header[function_doc_header.size() - 1] = '`';
	}

	if (is_prototype)
	{
		function_doc_header += " [PROTOTYPE]";
		return { function_doc_header, "" };
	}
	function_doc_header += '\n';
	
	// TODO: escape ` in body?
	return { function_doc_header, "```glsl\n" + tokens.back().first + "\n```\n" };
}

int main(int argc, char** argv)
{
	// ideally this should be a perfect hash map
	const std::unordered_set<std::string> allowed_ext = { ".h", ".glsl", ".frag", ".vert" };
	
	std::vector<std::string_view> input_dirs;
	std::filesystem::path output_dir = "";
	// output directory suffix, appended to output_dir
	std::vector<std::string_view> output_suffixes;
	// parse args
	{
		bool is_output_dir = false;
		bool is_output_suffix = false;
		std::size_t ind = 0;
		for (int i = 1; i < argc; i++)
		{
			using namespace std::string_view_literals;

			if (is_output_dir)
			{
				output_dir = argv[i];
				is_output_dir = false;
			}
			else if (is_output_suffix)
			{
				output_suffixes.back() = argv[i];
				is_output_suffix = false;
			}
			else if (argv[i] == "-o"sv)
			{
				is_output_dir = true;
			}
			else if (argv[i] == "--dir"sv)
			{
				is_output_suffix = true;
			}
			else
			{
				input_dirs.push_back(argv[i]);
				output_suffixes.emplace_back(); // add empty placeholder
			}
		}
	}
	
	for (std::size_t i = 0; i < input_dirs.size(); i++)
	{
		std::error_code ec;
		std::filesystem::path root = std::filesystem::canonical(input_dirs[i], ec);
		if (ec)
		{
			std::cerr << "Error processing " << input_dirs[i] << ":\n" << ec.message() << std::endl;
		}
		for (const auto& entry : std::filesystem::recursive_directory_iterator(root))
		{
			if (entry.is_directory())
			{
				continue;
			}
			if (!allowed_ext.contains(entry.path().extension().string()))
			{
				std::cout << "Skipping " << std::filesystem::relative(entry.path(), root) << " for unrecognized extension" << std::endl;
				continue;
			}
			try
			{
				using namespace std::literals::string_literals;
				// open input as binary and resolve line endings manually
				std::ifstream fin(entry.path(), std::ios::binary);
				std::filesystem::path output_path = output_dir / output_suffixes[i] / std::filesystem::relative(entry.path(), root).concat(".md");
				std::filesystem::path parent_path = output_path.parent_path();
				if (parent_path != "") { std::filesystem::create_directories(parent_path); }
				std::ofstream doc_file(output_path);
				
				// print name of file as header
				doc_file << "# " << entry.path().filename().string() << std::endl;
				// look for initial comment block, used to describe the file itself
				doc_file << read_comment<true>(fin) << std::flush;
				
				while (fin)
				{
					std::string comment = read_comment(fin);
					auto statement = read_statement(fin);
					// preprocessor
					{
						if (statement.size() == 1 && statement[0].second == token_type::preprocessor)
						{
							doc_file << "## Preprocessor: `" << statement[0].first << '`' << std::endl;
							continue;
						}
					}
					// variable
					{
						auto var_header = read_variable(statement);
						if (!var_header.empty())
						{
							doc_file << var_header;
							if (!comment.empty())
							{
								doc_file << comment;
							}
							doc_file << std::flush;
							continue;
						}
					}
					// function
					{
						auto [func_header, func_body] = read_function(statement);
						if (!func_header.empty())
						{
							doc_file << func_header;
							if (!comment.empty())
							{
								doc_file << comment;
							}
							doc_file << func_body << std::endl;
							continue;
						}
					}
					// skip lines we don't recognize (temporary)
					if (statement.empty())
					{
						if (!comment.empty())
						{
							doc_file << "## Comment\n";
						}
					}
					else
					{
						doc_file << "## UNKNOWN STATEMENT `";
						for (const auto& token : statement)
						{
							doc_file << token.first << ' ';
							// TODO: trailing space
						}
						doc_file << '`' << std::endl;
					}
					if (!comment.empty())
					{
						doc_file << comment << std::flush;
					}
				}
			}
			catch (const std::exception& e)
			{
				std::cerr << e.what() << std::endl;
			}
		}
	}
}
