# Compilation

GameLisp code is quite fast to load. [The Castle on Fire](../tcof/)'s codebase is currently around 
800 kilobytes of idiomatic GameLisp code (15 KLoC plus comments), which is completely loaded in 550 
milliseconds. For most games, it should be fine to simply call [`glsp::load`] at startup.

[`glsp::load`]: https://docs.rs/glsp/*/glsp/fn.load.html

However, if you're keen to improve startup performance, GameLisp supports pre-compilation of
its source code into a binary format, similar to Lua's binary chunks or Python's `py_compile`
module. Because this skips macro-expansion, it's much faster than [`glsp::load`]: The Castle
on Fire's source, when pre-compiled, loads in less than 100 milliseconds.

Compiling your source code can also help to obfuscate it. When you use compiled code, there's no 
need to distribute the original source files alongside your executable. Reverse-engineering the 
GameLisp binary format into readable GameLisp code would be a challenge, to say the least. 

Note that compiled GameLisp code does not run faster than uncompiled code. The final result is
the same - the only difference is how the code is loaded into memory.

Because compilation relies on the [`serde`](https://serde.rs/) and 
[`bincode`](https://docs.rs/bincode) crates, it's hidden behind the 
[feature flag](feature-flags.md) `"compiler"`, which is disabled by default.


## The `compile!` macro

The easiest way to precompile GameLisp code is using the `compile!` macro.

This is a procedural macro, so it runs while `rustc` is executing. It starts up a generic, empty
GameLisp runtime, uses it to compile all of the GameLisp source files which you specify, and
embeds the result directly into the Rust executable (like [`include_bytes!`]), returning it as a
`&'static [u8]`.

That byte slice can then be passed to [`glsp::load_compiled`] to run it.

[`compile!`]: https://docs.rs/glsp/*/glsp/macro.compile.html
[`include_bytes!`]: https://doc.rust-lang.org/std/macro.include_bytes.html
[`glsp::load_compiled`]: https://docs.rs/glsp/*/glsp/fn.load_compiled.html


```rust	
//these two calls are roughly equivalent
glsp::load_compiled(compile!["scripts/main.glsp"])?;
glsp::load("scripts/main.glsp")?;
```

The main downside of using [`compile!`] is that, because your scripts are no longer being loaded
from the filesystem, the only way to change them is to run `rustc` again, which is slow and
inconvenient. Therefore, you should generally only use [`compile!`] when producing your final
binary for distribution:

```rust
#[cfg(feature = "compiler")]
glsp::load_compiled(compile!["scripts/main.glsp"])?;

#[cfg(not(feature = "compiler"))]
glsp::load("scripts/main.glsp")?;
```


## The `glsp::load_and_compile` function

[`compile!`] is very convenient, but it always compiles your source files using an empty,
generic GameLisp runtime. This runtime will have access to the GameLisp standard library and any
macros you define using [`bind-macro!`](../std/bind-macro-mut) or [`defmacro`](../std/defmacro), 
but it won't run any of your Rust initialization code, so it won't have access to any libraries, 
any Rust functions, any assignments you've made to global variables from within Rust code, and 
so on.

This will only matter if you rely on one of your libraries, or call one of your Rust functions, 
in either of the following circumstances:
	
- When evaluating a toplevel form (see [Evaluation](evaluation.md))
- When running a macro expander

In the unlikely event that this is the case, your Rust program can perform its usual setup,
then compile GameLisp code manually by calling [`glsp::load_and_compile`]. This 
will return a `GResult<(Val, Vec<u8>)>`, where the `Val` is the result of loading and running 
the file, and the `Vec<u8>` is the result of compiling it.

It's straightforward to serialize a `Vec<u8>` to a file. The next time your program runs, you 
can read that `Vec<u8>` back in, and pass it to [`glsp::load_compiled`] as a byte slice.

The GameLisp binary format has absolutely no stability guarantees. If you recompile your 
executable, then you must also recompile any GameLisp binaries which that executable has produced 
in the past. Consider writing a [build script] which deletes any saved binaries.

[`glsp::load_and_compile`]: https://docs.rs/glsp/*/glsp/fn.load_and_compile.html
[build script]: https://doc.rust-lang.org/cargo/reference/build-scripts.html


## Corner Cases

GameLisp code is different from Lua or Python code, because it has a macro-expansion pass. It's
also different from Scheme and Rust, because those macros are defined *dynamically* - the set
of bound macros can change between one toplevel form and the next.

Macro-expansion is expensive, so the only way to efficiently compile GameLisp code is to expand
it before compiling it. This means that when you call [`glsp::load_compiled`], any macros
in the current GameLisp runtime will be ignored. All that matters is which macros were present in
the runtime which compiled the code.

Unfortunately, because of the dynamic binding mentioned above, the only way to macro-expand
GameLisp code is to run it. This is why the function is [`glsp::load_and_compile`] rather than 
just `glsp::compile`; one way of thinking about it is that we're loading the file (and all of the
files which it loads in turn), running it, and "recording" the execution in a format which can
be "played back" in the future.

99% of the time, you won't have to think about this. It's only relevant if the GameLisp environment
which calls [`glsp::load_and_compile`] somehow differs from the GameLisp environment which calls
[`glsp::load_compiled`] - because then the expected "playback" will differ from the actual 
"recording", and panics or logic bugs may occur.
	
	; because you're loading different files on different run-throughs, an 
	; error will occur if you compile this form on a linux machine and then 
	; run it on a non-linux machine.
	(cond
	  (on-linux?
	    (load "linux.glsp"))
	  (else
	    (load "non-linux.glsp")))
	
	; this macro expands to a constant value representing the screen width 
	; *at the time of macro expansion*. if this happens to be different between 
	; your own machine and the user's machine, the value will be incorrect.
	(defmacro screen-w ()
	  (my-window-library:screen-width))
	
	; because you're calling an rfn, this form will trigger an error if you pass
	; it to the compile![] macro, rather than glsp::load_and_compile.
	(my-sound-library:init)

The simplest way to protect yourself against this is to use [`compile!`] rather than
[`glsp::load_and_compile`], and perform all of your loading immediately after calling 
[`Runtime::new`], before binding libraries or doing anything else which might modify the 
[`Runtime`]. This means that you won't be able to access libraries or Rust functions from the 
toplevel or from macro expanders - but you will still be able to access them from within normal 
GameLisp functions.

[`Runtime`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html
[`Runtime::new`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html#method.new
	
	; this is fine, because it's a fn rather than a macro
	(defn screen-w ()
	  (my-window-library:screen-width))
	
	; this is fine, as long as (init-sound) isn't called from the toplevel or 
	; from a macro expander
	(defn init-sound ()
	  (my-sound-library:init))
