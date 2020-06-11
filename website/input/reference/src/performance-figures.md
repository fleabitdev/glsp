# Performance Figures

All performance timings on this page were obtained using a mid-range laptop from 2016 with
an Intel Core i7-6500U CPU (2.5 GHz). Rust code was compiled with `opt-level = 3`, `lto = true`,
`codegen-units = 1`. The `"unsafe-internals"` feature was enabled unless noted otherwise.


## Specimen Project

GameLisp's specimen project is [The Castle on Fire](https://gamelisp.rs/tcof/), a 2D sidescroller
which is still under development. We can use this project to provide some representative
performance figures for a real-world game which uses GameLisp.

The project has around 15,000 non-empty, non-comment lines of GameLisp code, which are loaded 
into the GameLisp runtime in about 550 milliseconds (or less than 100 milliseconds when 
[pre&#8209;compiled](compilation.md)).

The entity system, main loop and parts of the physics system are implemented in GameLisp. Each
entity receives a minimum of two method calls per frame. In a scene with 292 (mostly trivial)
entities, the total execution time spent running GameLisp code is ~2.0 milliseconds per frame, the 
memory pressure is 180 kilobytes of small allocations per frame, and the GC time is around 0.10 
milliseconds per frame, maintaining a 7 megabyte heap.

When we switch to a very busy scene (30 physics-enabled on-screen enemies with behaviour scripting 
and rendering, all controlled from GameLisp), the GameLisp execution time climbs to ~4.0 
milliseconds, the memory pressure to 350 kilobytes, and the GC time to 0.20 milliseconds. 
Most of this execution time is spent on the physics simulation - moving the hottest parts of the 
physics engine to Rust would be an easy optimization, if necessary.

If the heap size seems small, note that the game's total memory usage is a few dozen
megabytes. The 7 megabyte heap only contains memory managed directly by GameLisp, not including 
`rdata`.

When the game is run on a low-end laptop from 2011 (with a Pentium P6100), all GameLisp code
execution (including source-file loading and garbage-collection) experiences a constant-factor 
slowdown of around 2x to 4x relative to the i7-6500U. The game remains very playable.


## Benchmarks

GameLisp uses a bytecode interpreter, so I've benchmarked it against other interpreted languages -
specifically, the reference implementations of Lua and Python. Please note that JITted scripting 
language implementations, like LuaJIT, PyPy, V8, and SpiderMonkey, would be significantly faster. 
(However, integrating those libraries into a Rust game project would not be straightforward.)

The "GameLisp (SI)" column was compiled with the [default feature flags](feature-flags.md), and the 
"GameLisp (UI)" column was compiled with the `"unsafe-internals"` flag enabled.

The `primitive_x` benchmarks perform a basic operation (like addition or array indexing)
in an unrolled loop, while the remaining benchmarks try to imitate actual game code.

|Benchmark|Lua 5.3|GameLisp (UI)|Python 3.7.7|GameLisp (SI)|
|---------|:-----:|:-----------:|:----------:|:-----------:|
|`primitive-inc`|432.2ms|672.1ms|3710.3ms|1079.8ms|
|`primitive-arith`|534.0ms|535.1ms|2298.7ms|828.4ms|
|`primitive-call0`|258.3ms|631.9ms|740.2ms|1036.1ms|
|`primitive-call3`|592.3ms|994.0ms|955.7ms|1891.2ms|
|`primitive-array`|76.1ms|204.5ms|247.4ms|390.5ms|
|`primitive-table`|85.8ms|395.9ms|269.6ms|679.2ms|
|`primitive-field`|82.9ms|217.6ms|333.2ms|633.6ms|
|`primitive-method`|489.1ms|1275.1ms|838.7ms|2093.0ms|
|`rects`|384.0ms|1142.5ms|1234.7ms|2664.8ms|
|`flood_fill`|400.1ms|632.2ms|664.3ms|976.9ms|
|`rotation`|657.4ms|1015.3ms|1325.6ms|1843.9ms|

By default, GameLisp's performance is inferior to Python. If you've benchmarked your
GameLisp scripts and established that they're a performance bottleneck, switching on the
`"unsafe-internals"` flag will roughly double their performance. With that flag switched
on, GameLisp's performance currently hovers somewhere between Lua and Python.


## Optimizing GameLisp Code

Don't.

I really mean it. Other than occasionally thinking about the big-O complexity of your algorithms, 
you shouldn't waste any effort at all trying to make GameLisp run fast. Please don't be tempted.

The primary reason is that, as described in [Section 2](the-rust-api.md), GameLisp is very closely 
integrated with Rust. Here's the punchline to those benchmark figures above:

|Benchmark|Rust|GameLisp (UI)|GameLisp (SI)|
|---------|:--:|:-----------:|:-----------:|
|`primitive-inc`|44.1ms|672.1ms|1079.8ms|
|`primitive-arith`|126.7ms|535.1ms|828.4ms|
|`primitive-call0`|14.1ms|631.9ms|1036.1ms|
|`primitive-call3`|34.5ms|994.0ms|1891.2ms|
|`primitive-array`|12.1ms|204.5ms|390.5ms|
|`primitive-table`|259.5ms|395.9ms|679.2ms|
|`primitive-field`|10.3ms|217.6ms|633.6ms|
|`primitive-method`|10.4ms|1275.1ms|2093.0ms|
|`rects`|9.6ms|1142.5ms|2664.8ms|
|`flood_fill`|2.7ms|632.2ms|976.9ms|
|`rotation`|59.4ms|1015.3ms|1843.9ms|

Idiomatic Rust code will often be more than a hundred times faster than idiomatic GameLisp code. 
Even hand-optimized GameLisp code is usually dozens of times slower than a naive Rust 
implementation. Rust is incredibly fast.

As I mentioned at the [very beginning](introduction-for-rust-programmers.md), GameLisp is 
intended to be used for the messy, exploratory, frequently-changing parts of your codebase. 
Whenever I've been faced with a dilemma between making GameLisp fast and making it convenient, 
I've almost always chosen convenience.

This is why I've put so much effort into giving GameLisp a really nice Rust API. If some part of 
your GameLisp codebase has unacceptably poor performance, it's usually an easy task to "rewrite
it in Rust", in which case you'll immediately reap huge performance gains.

Here's a quick breakdown of the parts of a game codebase which are likely to be better-suited 
for either Rust or GameLisp, speaking from experience:

- For binary file-format parsing, rendering, audio mixing, image processing, vertex processing, 
  spatial data structures, collisions, physics simulation, and particle effects, Rust is the 
  obvious winner.
  	- That being said, you can definitely manage those parts of your engine using GameLisp. 
  	  Your `.zip` file parser might be implemented in Rust, but your resource manager could be 
  	  implemented in GameLisp. Your physics simulator might be implemented in Rust, but you 
  	  could also have a `PhysicsRect` mixin which hooks an entity into the physics system.

    - GameLisp can also be useful for prototyping. For example, if your game procedurally generates
      its levels, you could use GameLisp to freely experiment with different algorithms, and then 
      reimplement the final algorithm in Rust.

- Your main loop might belong in Rust, depending on how complicated it is.
	- If your game uses an entity-component system, the bulk of the ECS should definitely be 
	  implemented in Rust. Good performance is more-or-less the entire point of an ECS.
	  Consider implementing a `Script` component which owns a GameLisp object and invokes callbacks 
	  on it - or perhaps even one component for each callback, like `UpdateScript` and `DrawScript`.

- For entity behaviour scripting and cutscene scripting, GameLisp is the obvious choice.
