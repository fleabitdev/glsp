# Libraries

As a Rust game developer, you're probably already aware of Rust's stern and disapproving 
attitude towards global variables.

In order to prevent unsafe mutation and unsafe multithreading, safe Rust is forced to completely
forbid global mutable variables. They need to be wrapped in a [`thread_local!`], a [`RefCell`], a 
[`Mutex`], a [`lazy_static!`], or something else to shield you from the shared mutable data.

[`thread_local!`]: https://doc.rust-lang.org/std/macro.thread_local.html
[`RefCell`]: https://doc.rust-lang.org/std/cell/struct.RefCell.html
[`Mutex`]: https://doc.rust-lang.org/std/sync/struct.Mutex.html
[`lazy_static!`]: https://docs.rs/lazy_static/

This is strictly necessary to uphold Rust's invariants, but it always makes global variables
much less convenient to use. Typical use of a [`thread_local!`] variable is not a pretty sight:

```rust
thread_local! {
	pub(crate) static LOG_FILE: RefCell<Option<File>> = RefCell::new(None);
}

fn log_str(text: &str) {
	LOG_FILE.with(|ref_cell| {
		let mut option = ref_cell.borrow_mut();
		if let Some(ref mut file) = *option {
			file.write_all(text.as_bytes()).ok();
		}
	})
}
```

Contrast the equivalent C code:

```c
thread_local FILE* log_file;

void log_str(const char* text) {
	if (log_file) {
		assert(fputs(text, log_file));
	}
}
```

When programming a game, you'll sometimes encounter a part of your engine which seems "naturally
global" - a texture manager, an audio mixer, an entity database. There's only ever going 
to be one of it, and it's only ever going to be accessed from a single thread, but Rust still
forces you to keep it at arm's length.

You're given two options:
	
	- Use something like a [`lazy_static!`] [`RwLock`]. At minimum, this requires you to recite the
	  incantation `NAME.read().unwrap()` every time you access the global object. This is 
	  inconvenient, it makes the order of initialization/destruction less predictable, and it 
	  carries a non&#8209;trivial performance cost.

	- Allocate your "global" object on the stack when your program starts up, and pass borrowed
	  references down the callstack to any function which needs to access it. I would consider this 
	  an anti&#8209;pattern in Rust game development - passing a context reference into most 
	  function calls adds a lot of visual noise, and it sometimes causes borrow&#8209;checker 
	  headaches.
		- This is why functions like [`glsp::sym`] are free functions, rather than being invoked
		  as methods on some `&mut Glsp` "God object". An earlier version of the `glsp` crate
		  did work that way, but it made the library much less pleasant to use.

[`RwLock`]: https://doc.rust-lang.org/std/sync/struct.RwLock.html
[`glsp::sym`]: https://docs.rs/glsp/*/glsp/fn.sym.html

If neither of these options seem appealing, GameLisp offers an alternative.


## Libraries

A "library" is a Rust object which is owned by a GameLisp [`Runtime`]. Unlike [`RData`], libraries 
are singletons: for a given library type, you can only register one instance of that type in each 
[`Runtime`].
	
Libraries must implement the [`Lib`] trait. You can implement [`Lib`] manually, but it's usually 
better to use the [`lib!`] macro. 

To add an instance of a library to a [`Runtime`], call [`glsp::add_lib`], and to remove it, call 
[`glsp::take_lib`]. To temporarily borrow the library from the active [`Runtime`], simply call 
[`T::borrow`] or [`T::borrow_mut`], where `T` is the library's type. 

[`Runtime`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html
[`RData`]: https://docs.rs/glsp/*/glsp/struct.RData.html
[`Lib`]: https://docs.rs/glsp/*/glsp/trait.Lib.html
[`lib!`]: https://docs.rs/glsp/*/glsp/macro.lib.html
[`glsp::add_lib`]: https://docs.rs/glsp/*/glsp/fn.add_lib.html
[`glsp::take_lib`]: https://docs.rs/glsp/*/glsp/fn.take_lib.html
[`T::borrow`]: https://docs.rs/glsp/*/glsp/trait.Lib.html#method.borrow
[`T::borrow_mut`]: https://docs.rs/glsp/*/glsp/trait.Lib.html#method.borrow_mut

```rust
lib! {
	struct Textures {
		map: HashMap<Sym, RRoot<Texture>>
	}
}

fn init_textures() -> GResult<()> {
	glsp::add_lib(Textures::new())?;

	Ok(())
}

fn texture(id: Sym) -> GResult<RRoot<Texture>> {
	match Textures::borrow().map.get(&id) {
		Some(texture) => Ok(RRoot::clone(texture)),
		None => bail!("texture {} does not exist", id)
	}
}
```

Dynamic checks are used to uphold Rust's aliasing rules - for example, it's an error to call 
[`glsp::take_lib`] or [`T::borrow_mut`] for a library which is currently borrowed. When the
[`Runtime`] is dropped, each of its libraries will be dropped in the reverse order that they
were registered.

This is already a big improvement compared to [`lazy_static!`], but the real magic comes from
[function type conversions](rust-functions.md#type-conversions). When a function parameter is a
shared or mutable reference to a library type, calling the function from GameLisp will 
automatically borrow that library for the duration of the function call. If the `texture()` 
function above was intended to be called from GameLisp, we could rewrite it like this:

```rust
impl Textures {
	fn texture(&self, id: Sym) -> GResult<RRoot<Texture>> {
		match self.map.get(&id) {
			Some(texture) => Ok(RRoot::clone(texture)),
			None => bail!("texture {} does not exist", id)
		}
	}
}

glsp::bind_rfn("texture", rfn!(Textures::texture))?;
```

The nice thing about this style of function binding is that the `Texture::texture` method is 
equally usable from both GameLisp and Rust. There's no need to maintain a separate set of
"wrapper functions" - GameLisp handles the type translations for you.

Even generic functions are supported! You just need to select a single concrete type signature
when you pass the binding to [`glsp::bind_rfn`]:

[`glsp::bind_rfn`]: https://docs.rs/glsp/*/glsp/fn.bind_rfn.html

```rust
impl Textures {
	fn texture<S: ToSym>(&self, id: S) -> GResult<RRoot<Texture>> {
		let sym = id.to_sym()?;

		match self.map.get(&sym) {
			Some(texture) => Ok(RRoot::clone(texture)),
			None => bail!("texture {} does not exist", sym)
		}
	}
}

glsp::bind_rfn("texture", rfn!(Textures::texture::<Sym>))?;
```

Library parameters, like the `&self` parameter above, don't consume an argument. You would invoke
this function from GameLisp by calling `(texture id)`, with only one argument.

A function may have multiple library parameters:

```rust
impl Textures {
	fn draw_texture(
		&self,
		renderer: &mut Renderer,
		id: Sym,
		x: i32,
		y: i32
	) -> GResult<()> {
		match self.map.get(&id) {
			Some(texture) => renderer.draw(texture, x, y),
			None => bail!("texture {} does not exist", id)
		}
	}
}

glsp::bind_rfn("draw-texture", rfn!(Textures::draw_texture))?;
```

Assuming `Renderer` is a library type, you would invoke the above function from GameLisp as 
`(draw-texture id x y)`.


## Symbol Caching

If you're finicky about performance, function calls like `glsp::global("texture-count")`
might make you nervous. That function will invoke `glsp::sym("texture-count")` every
time it's called, performing a hash-table lookup to convert the string into a symbol. Surely it
would be better to cache the symbol somewhere, and call `glsp::global(TEXTURE_COUNT_SYM)`
instead?

(In 90% of cases, the answer to that question is "no, it doesn't matter"... but the last 10% can 
be quite important!)

Symbols can't be stored in `static` variables, because they're unique to a particular [`Runtime`]. 
If you want to cache a symbol as a global variable, it should be stored in a library instead.
	
```rust
struct Textures {
	//...

	pub nearest_neighbour_sym: Sym,
	pub bilinear_sym: Sym,
	pub trilinear_sym: Sym
}

impl Textures {
	fn new() -> GResult<Textures> {
		Textures {
			//...

			nearest_neighbour_sym: glsp::sym("nearest-neighbour")?,
			bilinear_sym: glsp::sym("bilinear")?,
			trilinear_sym: glsp::sym("trilinear")?
		}
	}
}
```

We provide the [`syms!`] macro to make this more straightforward. The macro defines a struct
for which every field is a Sym, with a constructor function named `new()` which initializes each
field by calling [`glsp::sym`], returning a [`GResult`].

[`syms!`]: https://docs.rs/glsp/*/glsp/macro.syms.html
[`GResult`]: https://docs.rs/glsp/*/glsp/type.GResult.html

```rust
syms! {
	pub (crate) struct Syms {
		pub(crate) nearest_neighbour: "nearest-neighbour",
		pub(crate) bilinear: "bilinear",
		pub(crate) trilinear: "trilinear"
	}
}

struct Textures {
	syms: Syms,

	//...
}

impl Textures {
	fn new() -> GResult<Textures> {
		Textures {
			syms: Syms::new()?,

			//...
		}
	}
}
```


## Multithreading

Because libraries are owned by a GameLisp [`Runtime`], they're inherently single-threaded. There's
no way to simultaneously refer to the same library from multiple threads.

This is usually what you want. Multithreading is complicated (even in Rust!), and it carries a 
performance impact. It can be quite liberating to know for a fact that `Textures::draw_texture`
won't wait for another thread, trigger a deadlock, or waste a few dozen CPU cycles locking a 
[`Mutex`].

As discussed in the [previous chapter](rust-data.md#multithreading), your escape hatch is
[`Arc`]. If you decide to use concurrency to optimize some part of your library, you can wrap 
it in an `Arc<T>` or `Arc<Mutex<T>>`, and share only that part of the library between threads.

[`Arc`]: https://doc.rust-lang.org/std/sync/struct.Arc.html
