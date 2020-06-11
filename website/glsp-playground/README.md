This crate is compiled into `glsp_playground_bg.wasm`, the GameLisp engine for the playground.

The crate itself is very small. It only provides a few functions, to be invoked from JavaScript:

- `initEngine(text, filename, randSeed)`, which creates a new `Runtime` (dropping any `Runtime` 
  which already exists), stores it globally, and initializes it by running the given `text` as 
  GameLisp code. It then asserts that various globals exist and type-checks them: `play:width`, 
  `play:height`, `play:title` and `play:blurb`.

  - `width()`, `height()`, `title()` and `blurb()` return the cached values of those globals.

- `update(dt)` runs the global `play:update` function in the current `Runtime`, and then calls
  `glsp::gc()`.

To produce `glsp-playground.wasm`, run:
	
	cargo build --release --target wasm32-unknown-unknown

	wasm-bindgen target/wasm32-unknown-unknown/release/glsp_playground.wasm \
	--out-dir output --target no-modules --no-typescript

	cp -t ../input/playground output/glsp_playground_bg.wasm output/glsp_playground.js

For the details of how all of the above works, see [the `wasm-bindgen` guide][0].

[0]: https://rustwasm.github.io/docs/wasm-bindgen/examples/without-a-bundler.html

We don't currently bother with `wasm-opt -Os` or `lto = true`, because together they only reduce 
the gzipped binary size from ~530kb to ~490kb. Not worth the extra compile time.