# Rust Data

It's possible to move arbitrary Rust data onto the GameLisp heap, so that GameLisp can interact
with it directly. The primitive type for Rust data in GameLisp is `rdata`, and it's represented 
in the Rust API using the [`RData`] struct. A `Root<RData>` can be used as an argument to, or the 
return value from, a Rust function.

	(let sprite (engine:load-sprite "goblin.png"))
	(ensure (rdata? sprite))
	(engine:draw sprite @x @y)

To move a value onto the GameLisp heap, simply call [`glsp::rdata(your_value)`]. It will return a
`Root<RData>` - a shared reference to `your_value`'s new memory location on the heap.

[`glsp::rdata`] requires its argument to implement the [`RStore`] trait. You could implement this 
manually, but it's usually much more convenient to use the [`rdata!`] macro, which also generates 
a few extra trait implementations to enable some of the useful features described below.
	
```rust
rdata! {
	struct Sprite {
		name: Sym,
		texture: ggez::graphics::Image
	}
}
```

[`RData`]: https://docs.rs/glsp/*/glsp/struct.RData.html
[`glsp::rdata(your_value)`]: https://docs.rs/glsp/*/glsp/fn.rdata.html
[`glsp::rdata`]: https://docs.rs/glsp/*/glsp/fn.rdata.html
[`RStore`]: https://docs.rs/glsp/*/glsp/trait.RStore.html
[`rdata!`]: https://docs.rs/glsp/*/glsp/macro.rdata.html


## The `RData` Type

[`RData`] is essentially a wrapper for `RefCell<Option<Rc<dyn Any>>>`, where the `dyn Any` is
actually a `RefCell<T>`.

This may seem a little baroque, but it gives [`RData`] a lot of nice features. In exchange for a 
few words of memory overhead, you automatically get [`Any`]‑style dynamic typing and 
[`RefCell`]‑style internal mutability:

[`Any`]: https://doc.rust-lang.org/std/any/trait.Any.html
[`RefCell`]: https://doc.rust-lang.org/std/cell/struct.RefCell.html
	
```rust
let rdata: Root<RData> = Root::clone(&sprite_bank.sprites["goblin"]);
let mut sprite = rdata.borrow_mut::<Sprite>();
sprite.name = glsp::sym("goblin")?;
```

If an [`RData`] is not currently borrowed, you can remove it from the GC heap and take back 
ownership using the [`take`] method. Any future attempts to refer to that `rdata` from 
GameLisp code will gracefully fail. This means that if you want to clean up some Rust data, 
you're not left at the mercy of the garbage collector.

[`take`]: https://docs.rs/glsp/*/glsp/struct.RData.html#method.take

```rust
let rdata: Root<RData> = sprite_bank.sprites.remove("goblin").unwrap();
let sprite: Sprite = rdata.take()?;
drop(sprite);
```

It's not always convenient to store a dynamically-typed [`Root<RData>`] when you know 
that the `RData` actually belongs to a specific concrete type. The [`RRoot<T>`] type behaves 
like a [`Root<RData>`], but without the type-erasure.

[`Root<RData>`]: https://docs.rs/glsp/*/glsp/struct.Root.html
[`RRoot<T>`]: https://docs.rs/glsp/*/glsp/struct.RRoot.html

```rust
//dynamically-typed, sometimes inconvenient
struct SpriteBank {
	sprites: HashMap<String, Root<RData>>
}

//more strongly-typed, more convenient
struct SpriteBank {
	sprites: HashMap<String, RRoot<Sprite>>
}
```

The [`rdata!`] macro will automatically implement [`MakeArg`] for shared and mutable references to 
your type, and [`IntoResult`] for the type itself. This enables it to seamlessly participate 
in [function type conversions](rust-functions.md#type-conversions):

[`rdata!`]: https://docs.rs/glsp/*/glsp/macro.rdata.html
[`MakeArg`]: https://docs.rs/glsp/*/glsp/trait.MakeArg.html
[`IntoResult`]: https://docs.rs/glsp/*/glsp/trait.IntoResult.html

```rust
fn load_sprite(path: &Path) -> Sprite {
	//the return value will be promoted to the heap using glsp::rdata()
	...
}

fn draw_sprite(engine: &mut Engine, sprite: &Sprite) {
	//the sprite's `RData` will be borrowed for the duration of the function
	...
}

impl Sprite {
	fn name(&self) -> Sym {
		self.name
	}
}

glsp::bind_rfn("engine:load-sprite", rfn!(load_sprite))?;
glsp::bind_rfn("engine:draw-sprite", rfn!(draw_sprite))?;
glsp::bind_rfn("engine:sprite-name", rfn!(Sprite::name))?;
```

Even better, the [`rdata!`] macro allows you to add methods and properties to your struct. They
behave just like the methods and properties on a [GameLisp object](object-oriented-programming.md).

```rust
rdata! {
	#[derive(Clone)]
	struct Sprite { 
		/* as above */
	}

	meths {
		get "name": Sprite::name,
		set "name": Sprite::set_name,
		get "width": Sprite::width,
		get "height": Sprite::height,
		"count-pixels": Sprite::count_pixels,
		"op-clone": Sprite::clone
	}
}

impl Sprite {
	fn name(&self) -> Sym {
		self.name
	}

	fn set_name(&mut self, new_name: Sym) {
		self.name = new_name;
	}

	fn width(&self) -> u16 {
		self.texture.width()
	}

	fn height(&self) -> u16 {
		self.texture.height()
	}

	fn count_pixels(&self) -> u32 {
		(self.width() as u32) * (self.height() as u32)
	}
}
```

<span></span>

	(let sprite (engine:load-sprite "goblin.png"))
	(ensure (== (.count-pixels sprite) (* [sprite 'width] [sprite 'height])))

	(= [sprite 'name] 'changeling)
	(prn [sprite 'name]) ; prints changeling

	; rdata can be indexed using iterables
	(prn ..[sprite '(width height)])

	; rdata participate in operator overloading
	(let cloned (clone sprite))

You can query whether an `rdata` belongs to a particular Rust type by calling, for example, 
[`(is? rdata 'Sprite)`](../std/is-p). The last argument should be a symbol which is identical 
to the name of your struct. That same symbol will be returned if you call 
[`(class-of rdata)`](../std/class-of).

Because [`RData`]'s classes are represented by a symbol, it's an error if you allocate [`RData`] of
multiple distinct types which share the same name. For example, you couldn't have both 
`audio::Clip` and `video::Clip`, even though their fully-qualified names are different.


## Internal References

Rust's type system will statically prevent [`Root<T>`] and [`RRoot<T>`] from being stored in an 
[`RData`]. This is inconvenient, but necessary - storing a root on the garbage-collected heap
would be very likely to lead to memory leaks.

[`Root<T>`]: https://docs.rs/glsp/*/glsp/struct.Root.html

For the time being, you'll need to store your [`Root<T>`] in a [library](libraries.md) instead, 
and refer to it indirectly - for example, using an integer ID.

```rust	
rdata! {
	//type error
	struct PhysicsBox {
		owner: Root<Obj>,
		coords: Rect
	}

	//better
	struct PhysicsBox {
		owner_id: u32,
		coords: Rect
	}
}
```


## Multithreading

GameLisp is single-threaded, because multithreading is primarily a performance optimization. 
Nobody is writing multithreaded game code for its beauty or its convenience! GameLisp code simply 
[isn't fast enough](performance-figures.md) for multithreading to be worth the extra complexity it
would add to the language, so it's not supported.

That being said, GameLisp is designed to be cleanly embedded into a multithreaded Rust program.
You can spin up a separate isolated [`Runtime`] for each thread if you like (similar to a 
[Web Worker]), but it would be more typical to have one [`Runtime`] which lives on the main 
thread, with a few worker threads which only run Rust code.

[`Runtime`]: https://docs.rs/glsp/*/glsp/struct.Runtime.html
[Web Worker]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API

Let's imagine you have a `Clip` which stores a few seconds of PCM audio data, and a worker thread
which performs audio mixing in software. You want to load and manipulate `Clips` from your GameLisp
scripts, while still allowing the worker thread to access their PCM samples. A naive attempt 
would look something like this:
	
```rust
rdata! {
	struct Clip {
		name: Sym,
		channels: Channels,
		samples: Vec<i16>
	}
}
```

You'll run into a problem when you try to pass your `Clip` to your worker thread. Because the
`Clip` contains a [`Sym`], which is non-[`Send`], you can't transfer it to another thread. In any
case, you can only refer to the `Clip` using the types [`Root<RData>`], [`RRoot<Clip>`], `&Clip`, and 
`&mut Clip`, none of which can be freely sent between threads.

Thanks to fearless concurrency, there's an easy fix. Simply wrap the shared parts of your
struct in an [`Arc<T>`]:

[`Sym`]: https://docs.rs/glsp/*/glsp/struct.Sym.html
[`Send`]: https://doc.rust-lang.org/std/marker/trait.Send.html
[`RRoot<Clip>`]: https://docs.rs/glsp/*/glsp/struct.RRoot.html
[`Arc<T>`]: https://doc.rust-lang.org/std/sync/struct.Arc.html

```rust
struct Samples {
	channels: Channels,
	samples: Vec<i16>
}

rdata! {
	struct Clip {
		name: Sym,
		samples: Arc<Samples>
	}
}

impl Clip {
	fn play(&self, mixer: &Mixer) {
		mixer.play_samples(Arc::clone(&self.samples));
	}
}
```

Ideally, when writing a multithreaded game with GameLisp, your worker threads shouldn't know that 
GameLisp exists at all. Think of each worker thread's code as a small library written in pure Rust, 
for which your main thread is a client. In the example above, the `Mixer` could be thought of as
a library which mixes raw sample buffers, *not* a library which mixes `Clips` specifically.
