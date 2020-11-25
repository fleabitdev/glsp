# The `glsp` Crate

Let's take another look at the skeleton project from the [Overview](overview.md) chapter:

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

The main player here is the [`Runtime` type](https://docs.rs/glsp/*/glsp/struct.Runtime.html). 
It owns all of the data required to run a single GameLisp instance: a garbage&#8209;collected heap, 
a symbol registry and a call stack, among other things. When the `Runtime` is dropped, all of that 
data is automatically freed.

There can be many simultaneous `Runtimes` in a program, and a `Runtime` can be created
on any thread, but `Runtimes` (and handles to the data inside them) can never be moved from one 
thread to another. Although GameLisp can be gracefully integrated into a multithreaded program, 
each individual GameLisp runtime is entirely single-threaded.


## The Active Runtime

Unusually for a Rust library, the `glsp` crate uses [`thread_local` variables][0] internally. 
This means that there's no need to pass around a context object (aka "God object"), because that 
would make the crate much less convenient to use.

[0]: https://doc.rust-lang.org/std/macro.thread_local.html

Instead, each thread has a hidden `thread_local` variable which points to its active `Runtime`. 
The `rt.run(...)` method sets `rt` to be the active `Runtime`, executes an arbitrary closure, 
and then restores the `Runtime` which was previously active, if any.

The `glsp` crate contains a large number of free functions, like [`glsp::sym`], which 
manipulate the active `Runtime` via that `thread_local` pointer. If these functions are called 
when no `Runtime` is active, they panic.

Most of the functions closely imitate an equivalent function which is built in to GameLisp.
For example, [`glsp::sym`] is equivalent to the [`sym` function](../std/sym), and [`glsp::load`] 
is equivalent to [`load`](../std/load).

[`glsp::sym`]: https://docs.rs/glsp/*/glsp/fn.sym.html
[`glsp::load`]: https://docs.rs/glsp/*/glsp/fn.load.html

Most games will have no need for multiple `Runtimes`. The simplest way to use GameLisp is to 
create a single `Runtime` at program start, and immediately make it active by calling its 
[`run()` method], passing in a closure which lasts for the entire duration of your program's
`main` function.

[`run()` method]: https://docs.rs/glsp/*/glsp/struct.Runtime.html#method.run

```rust
use glsp::prelude::*;

fn main() {
	let runtime = Runtime::new();
	runtime.run(|| {
		
		//...your entire program executes within this scope...

		Ok(())
	});
}
```


## Types

To introduce a few of the crate's most important types:

- [`Val`] is the enum which represents a GameLisp value. Each variant corresponds to 
  one of the sixteen [primitive types](syntax-and-types.md#type-summary).

- [`Sym`] represents a GameLisp symbol. It's a small `Copy` type which just wraps a
  `u32` identifier.

- [`Root`] is a smart pointer which refers to something stored on the garbage-collected 
  heap. It points to a struct which represents one of GameLisp's primitive types, such as 
  [`Root<Arr>`] for an array or [`Root<Coro>`] for a coroutine. Such types can only be accessed
  using a `Root`; your program will never take direct ownership of an [`Arr`] or a [`Coro`].

- [`GFn`] is the name for GameLisp's `fn` primitive type, to avoid confusion with Rust's
  `Fn` trait. Similarly, the GameLisp type `iter` is represented by the Rust type [`GIter`].

[`Val`]: https://docs.rs/glsp/*/glsp/enum.Val.html
[`Sym`]: https://docs.rs/glsp/*/glsp/struct.Sym.html
[`Root`]: https://docs.rs/glsp/*/glsp/struct.Root.html
[`Root<Arr>`]: https://docs.rs/glsp/*/glsp/struct.Arr.html
[`Arr`]: https://docs.rs/glsp/*/glsp/struct.Arr.html
[`Root<Coro>`]: https://docs.rs/glsp/*/glsp/struct.Coro.html
[`Coro`]: https://docs.rs/glsp/*/glsp/struct.Coro.html
[`GFn`]: https://docs.rs/glsp/*/glsp/struct.GFn.html
[`GIter`]: https://docs.rs/glsp/*/glsp/struct.GIter.html

### Moving Data Between Runtimes

Types which represent GameLisp data, like `Root`, `Val` and `Sym`, are closely linked to the
specific `Runtime` in which they were constructed. You shouldn't attempt to manipulate GameLisp 
data when there's no active `Runtime`, and you should never move GameLisp data from one `Runtime` 
to another.

For example, if you return a `Val` from `Runtime::run`, and then attempt to print it, your
program will panic. If you construct a symbol in one `Runtime`, and then attempt to print it
while a different `Runtime` is active, your program is likely to print an incorrect name.

Moving data between `Runtimes` is always memory-safe, but the results are otherwise undefined.
Under some circumstances, in order to preserve memory safety, GameLisp may be forced to 
[abort the process]!

[abort the process]: https://doc.rust-lang.org/std/process/fn.abort.html


## Generic Conversions

Functions and methods in the `glsp` crate tend to be highly generic, to keep manual type 
conversions to a minimum.

For example, this is the signature of the [`glsp::global`] function, which is designed
to imitate GameLisp's `(global)` function:

[`glsp::global`]: https://docs.rs/glsp/*/glsp/fn.global.html

```rust
pub fn global<S, T>(s: S) -> GResult<T> where
    S: ToSym,
    T: FromVal, 
```

The [`ToSym`] trait converts something to a symbol, and the [`FromVal`] trait is implemented by 
anything which can be fallibly converted from a [`Val`]. In practice, this means that the function 
can be called like this...

[`ToSym`]: https://docs.rs/glsp/*/glsp/trait.ToSym.html
[`FromVal`]: https://docs.rs/glsp/*/glsp/trait.FromVal.html
[`Val`]: https://docs.rs/glsp/*/glsp/enum.Val.html

```rust
let frames: u32 = glsp::global("frames")?;
```

...rather than needing a mess of explicit type conversions:

```rust
let frames = u32::from_val(&glsp::global(glsp::sym("frames")?)?)?;
```

The only downside is that with so many generic types, Rust's type inference will sometimes get 
confused. Rust doesn't yet allow you to [put type annotations wherever you please][1], so under
those circumstances, you'll usually need to introduce a temporary local variable with an
explicit type.

[1]: https://github.com/rust-lang/rfcs/pull/2522

In particular, for functions like [`glsp::call`] which have a generic return value, the type of
the return value must be specified explicitly, even when it's discarded.

[`glsp::call`]: https://docs.rs/glsp/*/glsp/fn.call.html
	
```rust
//an error
glsp::call(&my_gfn, &(1, 2, 3))?;

//correct
let _: Val = glsp::call(&my_gfn, &(1, 2, 3))?;
```


## Comprehensive Coverage

The `glsp` crate aims to be comprehensive: if it's possible to achieve something in GameLisp code,
it should also be possible to achieve it in Rust. It's usually possible for any GameLisp code to be
translated into (much uglier) Rust code line-by-line.
	
	(for plant in plants
	  (when (< [plant 'height] 100)
	    (.grow plant 10)))

```rust
let plants: Root<Arr> = glsp::global("plants")?;
for val in plants.iter() {
	let plant = Root::<Obj>::from_val(&val)?;
    let height: i32 = plant.get("height")?;
    if height < 100 {
        let _: Val = plant.call("grow", &(10,))?;
    }
}
```


## The Prelude

Importing the [`glsp::prelude::*`] module will pull in all of the most commonly-used names 
from the `glsp` crate, except for the free functions: `glsp::bind_global()` doesn't become 
`bind_global()`. 

[`glsp::prelude::*`]: https://docs.rs/glsp/*/glsp/prelude/index.html

I can't overstate how much more convenient this is, compared to manually adding and removing 
dozens of imports to every file. If name collisions occur, they can be disambiguated via renaming:
	
```rust
use glsp::prelude::*;
use num::Num as NumTrait;
use error_chain::bail as ec_bail;
```

If glob imports aren't to your taste, naturally there's nothing stopping you from importing
names individually instead.


## Sandboxing

[`Runtime::new()`] creates a default `Runtime`. It's also possible to use the 
[`RuntimeBuilder` struct] to configure a `Runtime` before creating it.

[`Runtime::new()`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html#method.new
[`RuntimeBuilder` struct]: https://docs.rs/glsp/*/glsp/struct.RuntimeBuilder.html

Currently, the only configuration setting is [`sandboxed`], which defaults to `false`. A sandboxed
`Runtime` does not provide any of the built-in GameLisp functions which access the filesystem - 
namely [`load`](../std/load), [`include`](../std/include) and [`require`](../std/require). 
Untrusted GameLisp code can get up to all sorts of mischief even without filesystem access, so 
you should still proceed with great caution when running it.

[`sandboxed`]: https://docs.rs/glsp/*/glsp/struct.RuntimeBuilder.html#method.sandboxed


## Output Streams

By default, [`prn`](../std/prn-fn) will print its output to [`std::io::Stdout`], and 
[`eprn`](../std/eprn) will print its output to [`std::io::Stderr`].

It's possible to replace those streams with an arbitrary [`std::io::Write`] type using the 
functions [`glsp::set_pr_writer`] and [`glsp::set_epr_writer`]. For example, to 
discard both streams:

[`std::io::Stdout`]: https://doc.rust-lang.org/std/io/struct.Stdout.html
[`std::io::Stderr`]: https://doc.rust-lang.org/std/io/struct.Stderr.html
[`std::io::Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
[`glsp::set_pr_writer`]: https://docs.rs/glsp/*/glsp/fn.set_pr_writer.html
[`glsp::set_epr_writer`]: https://docs.rs/glsp/*/glsp/fn.set_epr_writer.html

```rust
use std::io::sink;

glsp::set_pr_writer(Box::new(sink()));
glsp::set_epr_writer(Box::new(sink()));
```

Or to send output to a log file and also to the standard output streams:

```rust
use std::io::{self, stdout, stderr, Write};

struct Tee<A: Write, B: Write>(A, B);

impl<A: Write, B: Write> Write for Tee<A, B> {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		self.0.write_all(buf).and_then(|_| self.1.write_all(buf))?;
		Ok(buf.len())
	}

	fn flush(&mut self) -> io::Result<()> {
		self.0.flush().and_then(|_| self.1.flush())
	}
}

//defining a cloneable file handle is left as an exercise for the reader
let log_file = SharedFile::new("log.txt");
glsp::set_pr_writer(Box::new(Tee(log_file.clone(), stdout())));
glsp::set_epr_writer(Box::new(Tee(log_file.clone(), stderr())));
```

Unless you [manually override them](https://github.com/rust-lang/rust/issues/31343), the Rust 
macros `print!()`, `println!()`, `eprint!()` and `eprintln!()` will still print to the standard
output and standard error streams. As an alternative, you can use the macros [`pr!()`], [`prn!()`],
[`epr!()`] and [`eprn!()`] to print to the active `Runtime`'s `pr_writer` and `epr_writer`.

[`pr!()`]: https://docs.rs/glsp/*/glsp/macro.pr.html
[`prn!()`]: https://docs.rs/glsp/*/glsp/macro.prn.html
[`epr!()`]: https://docs.rs/glsp/*/glsp/macro.epr.html
[`eprn!()`]: https://docs.rs/glsp/*/glsp/macro.eprn.html
