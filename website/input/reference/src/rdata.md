# RData

So far, so good! With the information from the previous chapter, we can handle most Rust 
functions which deal with primitive types, standard-library types and built-in GameLisp 
types, binding those functions to GameLisp with very little boilerplate.

We've also learned how to provide automatic argument and return-value conversions for our own 
Rust types - perhaps representing a tuple struct as an array, or representing an enum as a symbol.

The next step is to start dealing with Rust types which *can't* be represented as a GameLisp
primitive. Many Rust types, such as the standard [`File` struct], can't really be converted into
any of the GameLisp primitive types. Some other types would be too expensive to convert back and 
forth on every function call - for example, Rust's standard [`IpAddr` struct] could be represented 
as a GameLisp string, but it wouldn't be a very efficient choice.

In those cases, we can use the [`rdata` primitive type]. `rdata` stands for "Rust data".
An `rdata` is a reference to an arbitrary Rust value which has been moved onto the GameLisp heap. 
It's represented in the Rust API using the [`Val::RData` enum variant] and the [`RData` struct].

There are no special conditions for constructing an `rdata`; GameLisp can take ownership
of any `'static` type. Simply pass your Rust value to the [`glsp::rdata` function], which will 
consume the value, wrap it in an `RData`, move it onto the garbage-collected heap, and 
return a `Root<RData>` which points to its new memory location.

[`File` struct]: https://doc.rust-lang.org/std/fs/struct.File.html
[`IpAddr` struct]: https://doc.rust-lang.org/std/net/enum.IpAddr.html
[`rdata` primitive type]: syntax-and-types.md#type-summary
[`Val::RData` enum variant]: https://docs.rs/glsp/*/glsp/enum.Val.html
[`RData` struct]: https://docs.rs/glsp/*/glsp/struct.RData.html
[`glsp::rdata` function]: https://docs.rs/glsp/*/glsp/fn.rdata.html

```rust
let file: File = File::open("example.txt")?;
let rdata: Root<RData> = glsp::rdata(file);
```

The `RData` wrapper is dynamically typed, like [`Any`], and dynamically borrowed, like 
[`RefCell`]. You can query an `RData`'s type by calling `is()`, and you can dynamically borrow 
its payload by calling `borrow()` or `borrow_mut()`. If you get the type wrong, or dynamically
borrow the payload in a way that violates [Rust's aliasing rules], those methods will gracefully
fail.

[`Any`]: https://doc.rust-lang.org/std/any/index.html
[`RefCell`]: https://doc.rust-lang.org/std/cell/struct.RefCell.html
[Rust's aliasing rules]: https://doc.rust-lang.org/std/cell/

```rust
ensure!(rdata.is::<File>());

let file = rdata.borrow::<File>();
prn!("{} bytes", file.metadata()?.len());

let wrong_type = rdata.borrow::<PathBuf>(); //an error
let aliasing_mut = rdata.borrow_mut::<File>(); //an error
```

When you call `glsp::rdata`, you're transferring ownership of your Rust data to the garbage 
collector. If the `rdata` becomes unreachable for any reason, it will automatically be deallocated.
This will drop the `rdata`'s payload, invoking its destructor.

Being at the mercy of the garbage collector isn't always desirable, so you can manually take 
back ownership of an `RData`'s payload using the [`RData::take` method]. If you attempt to 
borrow an `RData` after calling its `take()` method, the `borrow()` call will panic.

[`RData::take` method]: https://docs.rs/glsp/*/glsp/struct.RData.html#method.take

```rust
let file: File = rdata.take::<File>()?;

let already_taken = rdata.borrow::<File>(); //an error
```


## `RData` as Return Values

In the previous chapter, we described how `rfn`s can return any Rust type which implements the
[`IntoVal` trait]. That trait is used to automatically convert the function's return value
into a GameLisp value.

We provide a default implementation of `IntoVal` for any `'static` type. This implementation
simply passes its `self` argument to the `glsp::rdata` function, converting it into an `rdata`.
This means that if you return one of your own types from an `rfn`, it will "just work", even if
you haven't provided an explicit implementation of `IntoVal`.

```rust
struct Sprite {
	//...
}

impl Sprite {
	fn new(path: &str) -> Sprite {
		//...
	}
}

glsp::bind_rfn("Sprite", &Sprite::new)?;
```

```
(let goblin (Sprite "goblin.png"))

(prn (rdata? goblin)) ; prints #t
```

[`IntoVal` trait]: https://docs.rs/glsp/*/glsp/trait.IntoVal.html


## `RData` as Arguments

To receive an `rdata` as an argument to an `rfn`, you could use the type `Root<RData>` and
borrow it manually.

```rust
fn sprite_size(rdata: Root<RData>) -> GResult<(u32, u32)> {
	let sprite = rdata.try_borrow::<Sprite>()?;
	Ok((sprite.width, sprite.height))
}
```

However, this isn't very convenient. We provide a better alternative. When processing `rfn` 
parameters, if GameLisp encounters a reference to an unknown `'static` type, it will accept 
an `rdata` argument and attempt to borrow it as that type for the duration of the function call.

In other words, GameLisp will automatically write the above code on your behalf!

```rust
fn sprite_size(sprite: &Sprite) -> (u32, u32) {
	(sprite.width, sprite.height)
}
```

This means that you can write a normal Rust method with a `&self` or `&mut self` parameter, and 
then bind it as an `rfn` without needing to modify it at all.

```rust
impl Sprite {
	pub fn size(&self) -> (u32, u32) {
		(self.width, self.height)
	}
}

glsp::bind_rfn("sprite-size", &Sprite::size)?;
```

As you might expect, a `&mut` reference will attempt to mutably borrow an `rdata`, obeying
Rust's usual aliasing rules.

```rust
fn copy_pixels(dst: &mut Sprite, src: &Sprite, x: u32, y: u32) {
	//...
}

glsp::bind_rfn("copy-pixels", &copy_pixels)?;
```

```
(let goblin (Sprite "goblin.png"))
(let changeling (Sprite "human.png"))

; this call succeeds
(copy-pixels changeling goblin 0 0)

; this call fails - the Sprite can't be both mutably and 
; immutably borrowed at the same time
(copy-pixels goblin goblin 0 0)
```

By default, it's not possible for an arbitrary Rust type to be passed into an `rfn` by value; 
only shared and mutable references are supported. You can override this default by implementing 
`FromVal` for your type. This usually works best for `Copy` types.

```rust
impl FromVal for Point {
	fn from_val(val: &Val) -> GResult<Point> {
		match val {
			Val::RData(rdata) if rdata.is::<Point>() => {
				Ok(*rdata.try_borrow::<Point>()?)
			}
			val => bail!("expected a Point, received {}", val)
		}
	}
}
```


## `RRoot`

You'll sometimes need to store references to `RData`. For example, you might need to build
a hash table which you can use to look up `Sprites` by name.

You could consider storing `Root<RData>` in the hash table, like so:

```rust
struct Sprites {
	by_name: HashMap<String, Root<RData>>
}

let rdata = sprites.by_name.get("angry-sun").unwrap();
let sprite = rdata.borrow::<Sprite>();
```

However, `Root<RData>` can be a little awkward to use, because it's dynamically typed. Every 
time you call `is()`, `take()`, `borrow()` or `borrow_mut()`, you'll need to specify that 
you're dealing with a `Sprite`, rather than a non-specific `RData`. You might also accidentally 
store a non-`Sprite` in the hash table.

Instead, consider using the [`RRoot` smart pointer], which behaves like a `Root<RData>` but 
doesn't erase the `RData`'s actual type:

```rust
struct Sprites {
	by_name: HashMap<String, RRoot<Sprite>>
}

let rdata = sprites.by_name.get("angry-sun").unwrap();
let sprite = rdata.borrow();
```

[`RRoot` smart pointer]: https://docs.rs/glsp/*/glsp/struct.RRoot.html
