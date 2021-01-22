# Overview

[GameLisp](..)'s documentation is split into three parts:

- The Reference Manual, which you're currently reading, is a rough overview of the language. 
  It's written in an accessible style, so it doubles as a tutorial when read from beginning to end.

- The [standard library documentation](../std/) describes all of the special forms, functions and 
  macros which are built into the language itself. It covers most of the same material as 
  [Section 1](the-language.md) of the Reference Manual, but it's more formal, comprehensive 
  and precise.

- The [`glsp` crate documentation](https://docs.rs/glsp/) describes how to embed GameLisp
  into a Rust program. [Section 2](the-rust-api.md) of the Reference Manual will walk you 
  through the basics.


## Getting Started

If you haven't already, take a look at the 
[Introduction&nbsp;for Rust Programmers](introduction-for-rust-programmers.md) 
and/or the 
[Introduction&nbsp;for Lisp Programmers](introduction-for-lisp-programmers.md).

You can get a feel for the language by examining the source code for a few small games on 
the [interactive playground](../playground/).

To start setting up a GameLisp project of your own, check that you're running the latest version
of nightly Rust, and then add this line to your `Cargo.toml`:
    
    [dependencies]
    glsp = "0.2"

The following boilerplate code will load and run a single GameLisp source file named `main.glsp` in
the working directory, printing a stack trace if any errors occur. (The working directory is 
usually the same directory which contains your project's `Cargo.toml` file.)
    
```rust
use glsp::prelude::*;

fn main() {
    let runtime = Runtime::new();
    runtime.run(|| {
        glsp::load("main.glsp")?;
        Ok(())
    });
}
```

Once that's up and running, I'd recommend working your way through the Reference Manual from start
to finish. It's not too long - it can be completed in three hours or so. As you progress, you can 
use your skeleton project to experiment with the language, the standard library, and the Rust API.

Syntax-highlighting schemes for Visual Studio Code and Sublime Text 3 can be downloaded from the
[GitHub repository][0].

[0]: https://github.com/fleabitdev/glsp/tree/master/syntax-highlighting/

If you've never worked with Lisp before, you might find some parts of GameLisp difficult to 
understand. If you're struggling, consider looking through some beginner-level material for another 
Lisp dialect, such as the excellent [Practical Common Lisp](http://www.gigamonkeys.com/book/) by 
Peter Seibel.


## ⚠️ Stability Warning ⚠️

Because GameLisp is brand new, it currently provides **no stability guarantees**. The language, 
standard library and Rust API can and will change without notice. It's also immature enough that 
you may come across the occasional bug - if so, a [bug report][1] would be appreciated.

[1]: https://github.com/fleabitdev/glsp/issues/

GameLisp can still be used for serious game projects (I'm using it for [one](../tcof/) myself!), 
but you'll need to be prepared to refactor your codebase from time to time.
