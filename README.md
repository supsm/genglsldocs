# genglsldocs
Generate simple Markdown documentation for GLSL source files (may also work to an extent with C/C++ or related languages). It can only understand basic function and variable declarations (simple structs are planned). Please note this software is WIP. 

## Use
Pass a list of directories to parse as command-line arguments. All .vert, .frag, .glsl, and .h files are parsed, other file extensions must be specified in source. Directory structure is preserved.  
`-o <dir>` - specify global output directory (defaults to working directory)  
`--dir <dir>` - use after each input directory to specify an output directory relative to global output directory for said input (optional)  

Example: `genglsldocs path/to/src path/to/src2 --dir abc -o out`  
Documentation for path/to/src/util/noise.glsl is placed in out/util/noise.glsl.md, and path/to/src2/main.frag corresponds to out/abc/main.frag.md

## Demo
<details>
<summary><a href="https://github.com/vram-guild/frex/blob/1.19/common/src/main/resources/assets/frex/shaders/lib/math.glsl">Source file:</a></summary>

```glsl
/******************************************************
  frex:shaders/lib/math.glsl

  Commonly useful declarations and utilities.
  Use of these is entirely optional - half the fun
  is making your own.
******************************************************/

#define PI            3.1415926535897932384626433832795
#define HALF_PI    	  1.57079632679489661923 // I prefer a whole pi when I can get it, but I won't say no to half.
#define TAU           6.2831853071795864769252867665590 // two PI

/*
 * Has been around forever.  Gives a psuedorandom
 * hash value given two variables. Wouldn't be OK
 * for cryptography but may get the job done here.
 *
 * https://thebookofshaders.com/10/
 * https://stackoverflow.com/questions/12964279/whats-the-origin-of-this-glsl-rand-one-liner
 */
float frx_noise2d(vec2 st) {
	return fract(sin(dot(st.xy, vec2(12.9898, 78.233)))*43758.5453123);
}

/**
 *  Ken Perlin's improved smoothstep
 */
float frx_smootherstep(float edge0, float edge1, float x) {
	// Scale, and clamp to 0..1 range
	x = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
	// Evaluate polynomial
	return x * x * x * (x * (x * 6 - 15) + 10);
}

/**
 *  Ken Perlin's improved smoothstep
 */
vec3 frx_smootherstep(float edge0, float edge1, vec3 value) {
	// Scale, and clamp to 0..1 range
	vec3 r = clamp((value - edge0) / (edge1 - edge0), 0.0, 1.0);
	// Evaluate polynomial
	return r * r * r * (r * (r * 6 - 15) + 10);
}

/*
 * Animated 2d noise function,
 * designed to accept a time parameter.
 *
 * Based in part on 2D Noise by Morgan McGuire @morgan3d
 * https://www.shadertoy.com/view/4dS3Wd
 */
float frx_noise2dt (in vec2 st, float t) {
	vec2 i = floor(st);
	vec2 f = fract(st);

	// Compute values for four corners
	float a = frx_noise2d(i);
	float b = frx_noise2d(i + vec2(1.0, 0.0));
	float c = frx_noise2d(i + vec2(0.0, 1.0));
	float d = frx_noise2d(i + vec2(1.0, 1.0));

	a =  0.5 + sin((0.5 + a) * t) * 0.5;
	b =  0.5 + sin((0.5 + b) * t) * 0.5;
	c =  0.5 + sin((0.5 + c) * t) * 0.5;
	d =  0.5 + sin((0.5 + d) * t) * 0.5;

	// Mix 4 corners
	return mix(a, b, f.x) +
	(c - a)* f.y * (1.0 - f.x) +
	(d - b) * f.x * f.y;
}

/*
 * Converts RGB to grayscale.
 */
float frx_luminance(vec3 color) {
	return dot(color.rgb, vec3(0.299, 0.587, 0.114));
}
```
</details>
<details>
<summary>Generated documentation:</summary>
  
# math.glsl
****************************************************

frex:shaders/lib/math.glsl
Commonly useful declarations and utilities.
Use of these is entirely optional - half the fun
is making your own.
*****************************************************
## Preprocessor: `#define PI            3.1415926535897932384626433832795`
## Preprocessor: `#define HALF_PI    	  1.57079632679489661923 // I prefer a whole pi when I can get it, but I won't say no to half.`
## Preprocessor: `#define TAU           6.2831853071795864769252867665590 // two PI`
## `frx_noise2d(vec2 st)`

  Has been around forever.  Gives a psuedorandom
  hash value given two variables. Wouldn't be OK
  for cryptography but may get the job done here.


  https://thebookofshaders.com/10/
  https://stackoverflow.com/questions/12964279/whats-the-origin-of-this-glsl-rand-one-liner

```glsl
{
	return fract(sin(dot(st.xy, vec2(12.9898, 78.233)))*43758.5453123);
}
```

## `frx_smootherstep(float edge0, float edge1, float x)`


   Ken Perlin's improved smoothstep

```glsl
{
	// Scale, and clamp to 0..1 range
	x = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
	// Evaluate polynomial
	return x * x * x * (x * (x * 6 - 15) + 10);
}
```

## `frx_smootherstep(float edge0, float edge1, vec3 value)`


   Ken Perlin's improved smoothstep

```glsl
{
	// Scale, and clamp to 0..1 range
	vec3 r = clamp((value - edge0) / (edge1 - edge0), 0.0, 1.0);
	// Evaluate polynomial
	return r * r * r * (r * (r * 6 - 15) + 10);
}
```

## `frx_noise2dt(in vec2 st, float t)`

  Animated 2d noise function,
  designed to accept a time parameter.


  Based in part on 2D Noise by Morgan McGuire @morgan3d
  https://www.shadertoy.com/view/4dS3Wd

```glsl
{
	vec2 i = floor(st);
	vec2 f = fract(st);

	// Compute values for four corners
	float a = frx_noise2d(i);
	float b = frx_noise2d(i + vec2(1.0, 0.0));
	float c = frx_noise2d(i + vec2(0.0, 1.0));
	float d = frx_noise2d(i + vec2(1.0, 1.0));

	a =  0.5 + sin((0.5 + a) * t) * 0.5;
	b =  0.5 + sin((0.5 + b) * t) * 0.5;
	c =  0.5 + sin((0.5 + c) * t) * 0.5;
	d =  0.5 + sin((0.5 + d) * t) * 0.5;

	// Mix 4 corners
	return mix(a, b, f.x) +
	(c - a)* f.y * (1.0 - f.x) +
	(d - b) * f.x * f.y;
}
```

## `frx_luminance(vec3 color)`

  Converts RGB to grayscale.

```glsl
{
	return dot(color.rgb, vec3(0.299, 0.587, 0.114));
}
```
</details>
