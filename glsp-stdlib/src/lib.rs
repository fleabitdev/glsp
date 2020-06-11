#![forbid(unsafe_code)]

#![feature(proc_macro_hygiene)]

use glsp::{bail, Engine, EngineBuilder, Expander, GResult, GSend, lib, Lib, Sym};
use std::{i32, thread};
use std::collections::{hash_map::DefaultHasher, HashMap};
use std::hash::{Hash, Hasher};
use std::time::{Duration};

#[cfg(not(target_arch = "wasm32"))]
use std::time::{Instant, SystemTime};

mod class;
mod collections;
mod iter;
mod macros;
mod misc;
mod num;
mod pat;

lib! {
	pub(crate) struct Std {
		setters: HashMap<Sym, (Sym, bool)>,
		opt_setters: HashMap<Sym, (Sym, bool)>,
		classmacros: HashMap<Sym, Expander>,
		rng: Rng,

		#[cfg(not(target_arch = "wasm32"))]
		start_time: Instant
	}
}

impl Std {
	fn new() -> GResult<Std> {
		Ok(Std {
			setters: HashMap::new(),
			opt_setters: HashMap::new(),
			classmacros: HashMap::new(),
			rng: Rng::seeded(),

			#[cfg(not(target_arch = "wasm32"))]
			start_time: std::time::Instant::now()
		})
	}
}

//there are a handful of APIs we want to make available from Rust, but which make more intuitive
//sense as part of glsp-stdlib rather than glsp-engine - namely time and rng. we just define them 
//here as toplevel items which are re-exported by the `glsp` crate.

/** Equivalent to [`(time)`](https://gamelisp.rs/std/time). */

#[cfg(not(target_arch = "wasm32"))]
pub fn time() -> f32 {
	let std = Std::borrow();
	Instant::now().duration_since(std.start_time).as_secs_f32()
}

/** Equivalent to [`(sleep secs)`](https://gamelisp.rs/std/sleep). */

pub fn sleep(secs: f32) -> GResult<()> {
	//the Duration constructor will panic if secs is "not finite, negative, or overflows Duration"
	if secs.is_infinite() || secs.is_nan() || secs < 0.0 || secs as i32 > i32::MAX {
		bail!("{} is not an appropriate duration", secs);
	}

	thread::sleep(Duration::from_secs_f32(secs));
	Ok(())
}

pub(crate) struct Rng {
	x: u32,
	y: u32,
	z: u32,
	w: u32
}

impl Rng {
	fn seeded() -> Rng {
		let mut rng = Rng {
			x: 0,
			y: 0,
			z: 0,
			w: 0
		};

		//generate our initial seed based on the current time
		let mut hasher = DefaultHasher::default();

		#[cfg(target_arch = "wasm32")] {
			42u8.hash(&mut hasher);
		}

		#[cfg(not(target_arch = "wasm32"))] {
			let time = SystemTime::now();
			time.hash(&mut hasher);
		}
		
		rng.reseed(hasher.finish() as u32 as i32);

		rng
	}

	fn reseed(&mut self, mut seed: i32) {
		if seed == 0 {
			seed = 1
		}
		
		let x = seed as u32;
		let y = x.wrapping_mul(48271).wrapping_rem(0x7fffffff);
		let z = y.wrapping_mul(48271).wrapping_rem(0x7fffffff);
		let w = z.wrapping_mul(48271).wrapping_rem(0x7fffffff);

		*self = Rng { x, y, z, w };

		for _ in 0 .. 8 {
			self.gen_u32();
		}
	}
	
	fn gen_u32(&mut self) -> u32 {
		let Rng { x, y, z, w } = *self;

		let t = x ^ x.wrapping_shl(11);
		let new_w = w ^ w.wrapping_shr(19) ^ (t ^ t.wrapping_shr(8));

		self.x = y;
		self.y = z;
		self.z = w;
		self.w = new_w;

		new_w
	}
}

/** 
Uses GameLisp's [random number generator](https://gamelisp.rs/std/rand) to produce an `i32`.
*/
pub fn rand_i32() -> i32 {
	Std::borrow_mut().rng.gen_u32() as i32
}

/** 
Uses GameLisp's [random number generator](https://gamelisp.rs/std/rand) to produce an `f32`.
*/
pub fn rand_f32() -> f32 {
	//16777215 (0xffffff) is the largest continguous integer which can be exactly represented by 
	//an f32. seems like as good a choice as any other for our divisor. testing reveals that if 
	//the rng spits out 16777214u32, rand_float does generate 0.999something, rather than breaking 
	//the upper limit of our [0,1) range.
	let rand_u32 = Std::borrow_mut().rng.gen_u32() >> 8;
	(rand_u32 as f32) / 16777215.0f32
}

/** 
Uses GameLisp's [random number generator](https://gamelisp.rs/std/rand) to produce a `bool`.
*/
pub fn rand_bool() -> bool {
	//i don't necessarily trust the lowest bit of this rng, so we select a bit "randomly"
	let rand_u32 = Std::borrow_mut().rng.gen_u32();
	let to_shift = (rand_u32 & 0xf) + 4; //select from bits 4 through 20
	((rand_u32 >> (to_shift as usize)) & 1) == 1
}

/**
Equivalent to [`(rand-reseed seed)`](https://gamelisp.rs/std/rand-reseed).
*/
pub fn rand_reseed(seed: i32) {
	Std::borrow_mut().rng.reseed(seed)
}

/**
The GameLisp interpreter.

`Runtime` owns all of the data required to run GameLisp code: a set of global variables,
a symbol table, a set of registered Rust functions, and much more.

To manipulate the GameLisp runtime, you don't call methods on the `Runtime` type directly.
Instead, use the [`run` method](#method.run) to establish a particular `Runtime` as the
"active runtime", then use this crate's global functions and methods to interact with the
active runtime.

If you attempt to execute any GameLisp-related code without an active runtime, it will
almost always panic.

It's possible for multiple `Runtimes` to coexist. Each runtime is strictly isolated from the 
others - they don't share global variables, symbols, and so on. This means that it's possible
to run GameLisp code in isolated `Runtimes` on multiple threads without needing any 
synchronization.
*/

//a Runtime is just a thin wrapper for an Engine which has been initialized with the stdlib. we 
//separate Runtime from Engine so that procedural macros like backquote!(), which require a parser,
//can create an Engine while still being usable in the stdlib's implementation.
pub struct Runtime(Engine);

impl Runtime {

	/**
	Construct a `Runtime` with default settings.

	To construct a custom `Runtime`, use [`RuntimeBuilder`](struct.RuntimeBuilder.html)
	instead.
	*/
	pub fn new() -> Runtime {

		//Runtime::new() is currently a little slow: it costs about 0.4ms, of which 0.1ms is spent
		//constructing the Engine (especially the stock syms database) and 0.3ms is spent in 
		//init_stdlib. we could potentially get this down to the sub-0.1ms range by caching the
		//initial syms and rfns databases globally so that they can be clone()d. (todo?)

		//an obstacle in the way of that plan: Sym, Rc<str> and RFn are all !Send. the only way
		//to store them globally would be thread_local!{}, which would eliminate any performance
		//savings when creating the first Runtime of a thread... which is the main use-case for
		//making Runtime::new() fast in the first place! postponing this for now.

		RuntimeBuilder::new().build()
	}

	fn with_settings(sandboxed: bool, engine: Engine) -> Runtime {
		engine.run(|| {
			init_stdlib(sandboxed)
		}).unwrap();

		Runtime(engine)
	}

	/**
	Establish this `Runtime` as the active runtime.

	For the duration of the `f` closure, any calls to global functions like 
	[`glsp::sym`](fn.sym.html) or [`glsp::set_global`](fn.set_global.html) will manipulate 
	this `Runtime`.
	*/
	pub fn run<F, R>(&self, f: F) -> Option<R> 
	where
		F: FnOnce() -> GResult<R>,
		F: GSend,
		R: GSend
	{
		self.0.run(f)
	}
}

/**
Configuration options for constructing a [`Runtime`](struct.Runtime.html).

Currently, the only option is [`sandboxed`](#method.sandboxed).
*/
pub struct RuntimeBuilder {
	sandboxed: bool,
	engine_builder: EngineBuilder
}

impl RuntimeBuilder {
	pub fn new() -> RuntimeBuilder {
		RuntimeBuilder {
			sandboxed: false,
			engine_builder: EngineBuilder::new()
		}
	}

	/**
	Sets the `sandboxed` configuration option, which defaults to `false`.

	When `sandboxed` is `true`, the global functions [`load`](https://gamelisp.rs/std/load),
	[`require`](https://gamelisp.rs/std/require) and [`include`](https://gamelisp.rs/std/include)
	are not defined in the GameLisp runtime. It's still possible for Rust code to call 
	[`glsp::load`](fn.load.html) or [`glsp::require`](fn.require.html).
	*/
	pub fn sandboxed(self, sandboxed: bool) -> RuntimeBuilder {
		RuntimeBuilder {
			sandboxed,
			..self
		}
	}

	///Construct a `Runtime` with these settings.
	pub fn build(self) -> Runtime {
		Runtime::with_settings(self.sandboxed, self.engine_builder.build())
	}
}

fn init_stdlib(sandboxed: bool) -> GResult<()> {
	glsp::add_lib(Std::new()?);

	class::init(sandboxed)?;
	collections::init(sandboxed)?;
	iter::init(sandboxed)?;
	macros::init(sandboxed)?;
	misc::init(sandboxed)?;
	num::init(sandboxed)?;

	glsp::freeze_transform_fns();

	Ok(())
}
