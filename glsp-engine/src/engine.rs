use fnv::{FnvHashMap};
use owning_ref::{OwningHandle};
use self::stock_syms::*;
use smallvec::{SmallVec};
use std::{fmt, fs, str, u32};
use std::any::{Any, TypeId, type_name};
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::cmp::{Ordering};
use std::collections::{HashMap, hash_map::Entry::{Occupied, Vacant}, HashSet};
use std::convert::{TryFrom};
use std::fmt::{Debug, Display, Formatter, Pointer};
use std::io::{self, stderr, stdout, Write};
use std::iter::{FromIterator};
use std::marker::{PhantomData};
use std::num::{NonZeroU32};
use std::ops::{Deref, DerefMut};
use std::panic::{self, AssertUnwindSafe};
use std::path::{PathBuf};
use std::rc::{Rc};
use std::time::{SystemTime, UNIX_EPOCH};
use super::{eval, lex};
use super::class::{Class, Obj};
use super::code::{Coro, GFn};
use super::collections::{Arr, DequeAccess, DequeOps, IntoElement, Str, Tab};
use super::error::{GResult};
use super::eval::{Env, EnvMode, Expander, Expansion};
use super::gc::{Allocate, Heap, Gc, GcHeader, Slot, Root, Visitor};
use super::iter::{GcCallable, GIter, GIterState, Iterable, IterableOps};
use super::parse::{Parser};
use super::transform::{KnownOp, known_ops};
use super::val::{Num, Val};
use super::vm::{Frame, GlspApiName, Vm};
use super::wrap::{FromVal, ToCallArgs, Callable, CallableOps, ToVal, WrappedFn};

#[cfg(feature = "compiler")]
use std::mem::forget;

#[cfg(feature = "compiler")]
use super::{code::Stay, compile::{Action, Recording}};


//-------------------------------------------------------------------------------------------------
// ACTIVE_ENGINE
//-------------------------------------------------------------------------------------------------

thread_local! {
	static ACTIVE_ENGINE: RefCell<Option<Rc<EngineStorage>>> = RefCell::new(None);
	pub(crate) static ACTIVE_ENGINE_ID: Cell<Option<u8>> = Cell::new(None);

	//a bitset used for assigning engine ids. one bit for each possible id; the bit is set if an 
	//engine is currently using that id. it's an error for more than 256 engines to coexist
	//within a single thread.
	static ID_BITSET: [Cell<u32>; 8] = [
		Cell::new(0), Cell::new(0), Cell::new(0), Cell::new(0), 
		Cell::new(0), Cell::new(0), Cell::new(0), Cell::new(0)
	]; 
}

fn alloc_engine_id() -> Option<u8> {
	ID_BITSET.with(|bitset| {
		for i in 0 .. 8 {
			let mut bits = bitset[i].get();
			let mut j = 0;
			if bits != 0xffffffff {
				while bits & 0x1 == 0x1 {
					bits >>= 1;
					j += 1;
				}

				assert!(j <= 31 && i*32 + j <= 255);
				bitset[i].set(bitset[i].get() | (1 << j));

				return Some((i*32 + j) as u8)
			}
		}

		None
	})
}

fn free_engine_id(id: u8) {
	ID_BITSET.with(|bitset| {
		let i = id as usize / 32;
		let j = id as u32 % 32;

		assert!(bitset[i].get() & (0x1 << j) != 0);
		bitset[i].set(bitset[i].get() & !(0x1 << j));
	})
}

#[inline(always)]
fn with_engine<R, F: FnOnce(&EngineStorage) -> R>(f: F) -> R {
	ACTIVE_ENGINE.with(|ref_cell| {
		let borrow = ref_cell.borrow();
		let rc = borrow.as_ref().expect(
			"no glsp runtime is active; consider calling Runtime::run()"
		);

		f(&rc)
	})
}

#[inline(always)]
pub(crate) fn with_heap<R, F: FnOnce(&Heap) -> R>(f: F) -> R {
	ACTIVE_ENGINE.with(|ref_cell| {
		let opt_rc = ref_cell.borrow();
		match &*opt_rc {
			Some(rc) => f(&rc.heap),
			None => panic!("no glsp runtime is active; consider calling Runtime::run()")
		}
	})
}

#[inline(always)]
pub(crate) fn with_vm<R, F: FnOnce(&Vm) -> R>(f: F) -> R {
	ACTIVE_ENGINE.with(|ref_cell| {
		let opt_rc = ref_cell.borrow();
		match &*opt_rc {
			Some(rc) => f(&rc.vm),
			None => panic!("no glsp runtime is active; consider calling Runtime::run()")
		}
	})
}

#[inline(always)]
pub(crate) fn with_known_ops<R, F: FnOnce(&HashMap<Sym, KnownOp>) -> R>(f: F) -> R {
	ACTIVE_ENGINE.with(|ref_cell| {
		let opt_rc = ref_cell.borrow();
		match &*opt_rc {
			Some(rc) => f(&rc.known_ops),
			None => panic!("no glsp runtime is active; consider calling Runtime::run()")
		}
	})
}

#[doc(hidden)]
pub fn with_lazy_val<R, FInit, FUse>(key: &str, f_init: FInit, f_use: FUse) -> R
where
	FInit: FnOnce() -> Val,
	FUse: FnOnce(&Val) -> R
{
	//we access the HashMap twice, rather than using the Entry api, because we don't want the
	//lazy_storage to be borrowed while f_init or f_use are executing - it's possible that
	//they might recursively call something which, in turn, calls glsp::with_lazy_val. 
	//(for example, this could definitely occur with the eval!() macro.)
	
	let val = with_engine(|engine| {
		engine.lazy_storage.borrow().get(key).cloned()
	});

	if let Some(val) = val {
		f_use(&val)
	} else {
		let new_val = f_init();
		with_engine(|engine| {
			engine.lazy_storage.borrow_mut().insert(key.to_string(), new_val.clone());
		});
		f_use(&new_val)
	}
}


//-------------------------------------------------------------------------------------------------
// pr!(), prn!(), epr!(), eprn!()
//-------------------------------------------------------------------------------------------------

//we can't have the macros call rt::with_pr_writer directly, because their arguments might use
//the ? operator. we use PrWriter and EprWriter as (slightly inefficient) adapters instead.
#[doc(hidden)]
pub struct PrWriter;

impl Write for PrWriter {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		with_engine(|engine| engine.pr_writer.borrow_mut().write(buf))
	}

	fn flush(&mut self) -> io::Result<()> {
		with_engine(|engine| engine.pr_writer.borrow_mut().flush())
	}
}

#[doc(hidden)]
pub struct EprWriter;

impl Write for EprWriter {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		with_engine(|engine| engine.epr_writer.borrow_mut().write(buf))
	}

	fn flush(&mut self) -> io::Result<()> {
		with_engine(|engine| engine.epr_writer.borrow_mut().flush())
	}
}

/**
Prints to the active [`Runtime`](struct.Runtime.html)'s current 
[`pr_writer`](fn.set_pr_writer.html).

The input syntax is identical to [`print!`](https://doc.rust-lang.org/std/macro.print.html).

The `pr_writer` defaults to [`Stdout`](https://doc.rust-lang.org/std/io/struct.Stdout.html).
*/
#[macro_export]
macro_rules! pr {
	($($arg:tt)*) => (
		{ 
			use std::io::Write;
			write!($crate::PrWriter, $($arg)*).ok();
			$crate::PrWriter.flush().ok();
		}
	);
}

/**
Prints to the active [`Runtime`](struct.Runtime.html)'s current 
[`pr_writer`](fn.set_pr_writer.html), with a trailing `'\n'`.

The input syntax is identical to [`println!`](https://doc.rust-lang.org/std/macro.println.html).

The `pr_writer` defaults to [`Stdout`](https://doc.rust-lang.org/std/io/struct.Stdout.html).
*/
#[macro_export]
macro_rules! prn {
	($($arg:tt)*) => (
		{
			use std::io::Write;
			writeln!($crate::PrWriter, $($arg)*).ok();
		}
	);
}

/**
Prints to the active [`Runtime`](struct.Runtime.html)'s current 
[`epr_writer`](fn.set_epr_writer.html).

The input syntax is identical to [`eprint!`](https://doc.rust-lang.org/std/macro.eprint.html).

The `epr_writer` defaults to [`Stderr`](https://doc.rust-lang.org/std/io/struct.Stderr.html).
*/
#[macro_export]
macro_rules! epr {
	($($arg:tt)*) => (
		{ 
			use std::io::Write;
			write!($crate::EprWriter, $($arg)*).ok();
			$crate::EprWriter.flush().ok();
		}
	);
}

/**
Prints to the active [`Runtime`](struct.Runtime.html)'s current 
[`epr_writer`](fn.set_epr_writer.html), with a trailing `'\n'`.

The input syntax is identical to [`eprintln!`](https://doc.rust-lang.org/std/macro.eprintln.html).

The `epr_writer` defaults to [`Stderr`](https://doc.rust-lang.org/std/io/struct.Stderr.html).
*/
#[macro_export]
macro_rules! eprn {
	($($arg:tt)*) => (
		{
			use std::io::Write;
			writeln!($crate::EprWriter, $($arg)*).ok();
		}
	);
}


//-------------------------------------------------------------------------------------------------
// EngineStorage, EngineBuilder and Engine
//-------------------------------------------------------------------------------------------------

#[doc(hidden)]
pub struct EngineBuilder;

impl EngineBuilder {
	pub fn new() -> EngineBuilder {
		EngineBuilder
	}

	pub fn build(self) -> Engine {
		Engine::new()
	}
}

#[doc(hidden)]
pub struct Engine(Rc<EngineStorage>);

impl Drop for Engine {
	fn drop(&mut self) {
		/*
		we begin by freeing all RData which haven't already been freed, in an arbitrary order.
		freeing RData before we drop any Libs makes RData destructors more ergonomic to write
		(for example, this makes it possible for a Sprite rdata to deregister itself from 
		the Graphics lib when it's dropped).

		we then drop all of the Libs in the reverse order that they were registered.
		we allow take_lib() and add_lib() to be called in a Lib destructor by leaving 
		engine.libs and engine.libs_ordering unborrowed while the destructor runs.

		finally, we clean up the Heap, make this Engine inactive, and then assert that the 
		EngineStorage will be dropped when this Engine is dropped.
		*/

		self.run(|| {
			with_engine(|engine| {

				//free rdata. (todo: should probably find a more efficient way to handle the
				//corner case where an RData destructor allocates another RData)
				loop {
					let to_free = engine.heap.all_unfreed_rdata();
					if to_free.len() == 0 {
						break
					}

					for rdata in to_free {
						if !rdata.is_freed() {

							//free() will return an Err if the RData is currently borrowed. in
							//that case, we panic, because a Root should not exist while its Heap
							//is being dropped. (todo: should we check this invariant explicitly?)
							rdata.free().unwrap();
						}
					}
				}

				//drop libraries
				while let Some(type_id) = engine.libs_ordering.borrow_mut().pop() {
					let lib = engine.libs.borrow_mut().remove(&type_id).unwrap();
					drop(lib);
				}

				/*
				we need to clean up the Heap because otherwise it will leak memory if the 
				unsafe-internals flag is disabled, by leaving Rc reference loops intact. as 
				such, we first need to clean up anything that holds a Root. (this also helps
				us to uphold the invariant that a Root cannot exist when its Heap is dropped.)
				*/

				engine.lazy_storage.borrow_mut().clear();
				engine.syms.borrow_mut().clear();
				engine.rfns.borrow_mut().clear();
				engine.vm.clear();
				engine.heap.clear();
			});

			Ok(())
		});

		free_engine_id(self.0.id);

		//this should be guaranteed, i think, since engine.run() borrows the Engine so that it
		//can't be dropped, and there should be no other source for an Rc<EngineStorage>.
		debug_assert!(Rc::strong_count(&self.0) == 1);
	}
}

struct EngineStorage {
	id: u8,
	heap: Heap,
	vm: Vm,

	pr_writer: RefCell<Box<dyn Write>>,
	epr_writer: RefCell<Box<dyn Write>>,

	syms: RefCell<Vec<SymEntry>>,
	syms_map: RefCell<HashMap<Rc<str>, Sym>>,
	gensym_counter: Cell<u32>,
	gensym_seed: RefCell<Option<String>>,

	spans: RefCell<Vec<SpanStorage>>,
	spans_map: RefCell<HashMap<SpanStorage, Span>>,

	filenames: RefCell<Vec<Rc<str>>>,
	filenames_map: RefCell<HashMap<Rc<str>, Filename>>,
	required: RefCell<HashSet<PathBuf>>,

	rfns: RefCell<Vec<RFnEntry>>, 
	rfns_map: RefCell<HashMap<usize, RFn>>,
	rclasses: RefCell<HashMap<TypeId, Rc<RClass>>>,
	rclass_names: RefCell<HashSet<&'static str>>,

	in_expander: RefCell<Option<(Option<Sym>, Span, Rc<Env>)>>,
	errors_verbose: Cell<bool>,

	libs: RefCell<HashMap<TypeId, Rc<dyn Any>>>,
	libs_ordering: RefCell<Vec<TypeId>>,

	#[cfg(feature = "compiler")] recording: RefCell<Option<Recording>>,
	#[cfg(feature = "compiler")] playing_back: RefCell<Option<Recording>>,

	lazy_storage: RefCell<HashMap<String, Val>>,

	known_ops: HashMap<Sym, KnownOp>
}

struct SymEntry {
	name: Rc<str>,
	kind: SymKind,
	bound_global: Option<GlobalEntry>,
	bound_macro: Option<Expander>
}

struct GlobalEntry {
	val: Val,
	frozen: bool
}

struct RFnEntry {
	name: Option<Sym>,
	wrapped_fn: WrappedFn
}

impl Engine {
	pub fn new() -> Engine {
		//build the initial syms database
		let syms = Vec::from_iter(STOCK_SYMS.iter().map(|&(name, kind)| {
			SymEntry {
				name: name.into(),
				kind,
				bound_global: None,
				bound_macro: None
			}
		}));
		
		let syms_map = HashMap::from_iter(syms.iter().enumerate().map(|(i, sym_entry)| {
			(Rc::clone(&sym_entry.name), Sym(i as u32, PhantomData))
		}));

		//the initial spans database just contains "Generated", so that it's always Span(0)
		let spans = vec![SpanStorage::Generated];
		let mut spans_map = HashMap::new();
		spans_map.insert(SpanStorage::Generated, Span(0));

		//Filenames and RFns store a NonZeroU32 to enable some layout optimizations. therefore,
		//we need to insert dummy entries for Filename(0) and RFn(0).
		let rfns = vec![RFnEntry {
			name: None,
			wrapped_fn: rfn!(|| panic!())
		}];
		let filenames = vec!["".into()];

		let engine = Engine(Rc::new(EngineStorage {
			id: alloc_engine_id().expect("more than 256 simultaneous Runtimes"),
			heap: Heap::new(),
			vm: Vm::new(),

			pr_writer: RefCell::new(Box::new(stdout())),
			epr_writer: RefCell::new(Box::new(stderr())),

			syms: RefCell::new(syms),
			syms_map: RefCell::new(syms_map),
			gensym_counter: Cell::new(0),
			gensym_seed: RefCell::new(None),

			spans: RefCell::new(spans),
			spans_map: RefCell::new(spans_map),

			filenames: RefCell::new(filenames),
			filenames_map: RefCell::new(HashMap::new()),
			required: RefCell::new(HashSet::new()),

			rfns: RefCell::new(rfns),
			rfns_map: RefCell::new(HashMap::new()),
			rclasses: RefCell::new(HashMap::new()),
			rclass_names: RefCell::new(HashSet::new()),

			in_expander: RefCell::new(None),
			errors_verbose: Cell::new(true),

			libs: RefCell::new(HashMap::new()),
			libs_ordering: RefCell::new(Vec::new()),

			#[cfg(feature = "compiler")] recording: RefCell::new(None),
			#[cfg(feature = "compiler")] playing_back: RefCell::new(None),

			lazy_storage: RefCell::new(HashMap::new()),

			known_ops: known_ops()
		}));

		engine
	}

	//once we have specialisation, we could permit the closure to return any value, and only
	//have the error-reporting behaviour when the return type is GResult<_>.
	pub fn run<F, R>(&self, f: F) -> Option<R> 
	where
		F: FnOnce() -> GResult<R>
	{
		let old_active_engine = ACTIVE_ENGINE.with(|ref_cell| {
			ref_cell.replace(Some(self.0.clone()))
		});

		let old_engine_id = ACTIVE_ENGINE_ID.with(|cell| {
			cell.replace(Some(self.0.id))
		});

		let _guard = Guard::new(|| {
			ACTIVE_ENGINE.with(|ref_cell| {
				ref_cell.replace(old_active_engine);
			});
			ACTIVE_ENGINE_ID.with(|cell| {
				cell.set(old_engine_id);
			});
		});

		let result = f();

		if let Err(ref error) = result {
			if error.stack_trace().is_some() {
				eprn!("\nunhandled error in run() call:\n\n{}", error);
			} else {
				eprn!("\nunhandled error in run() call: {}", error);
			}
		}

		result.ok()
	}
}


//-------------------------------------------------------------------------------------------------
// Guard
//-------------------------------------------------------------------------------------------------

//because rfns are allowed to panic, and because various glsp functions can early-exit when an
//error is generated, we need to be careful about unwind-safety. we mostly achieve this using the
//Guard struct, which runs an arbitrary closure when it's dropped.
pub(crate) struct Guard<F: FnOnce()>(Option<F>);

impl<F: FnOnce()> Guard<F> {
	pub(crate) fn new(f: F) -> Guard<F> {
		Guard(Some(f))
	}
}

impl<F: FnOnce()> Drop for Guard<F> {
	fn drop(&mut self) {
		(self.0.take().unwrap())()
	}
}


//-------------------------------------------------------------------------------------------------
// Sym, ToSym, RFn, Filename
//-------------------------------------------------------------------------------------------------

/**
The `sym` primitive type.

Symbols are represented by a small `Copy` type (a 32-bit integer id).

To convert a string into a symbol, you should usually call [`glsp::sym`](fn.sym.html).
*/

//the PhantomData is used to ensure that Syms are !Send and !Sync

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct Sym(pub(crate) u32, pub(crate) PhantomData<*mut ()>);

const MAX_SYM: u32 = 0xffffff;

impl Sym {
	/**
	Returns the name of the symbol.

	For gensyms, the name is the same as its printed representation, e.g. `#<gs:tag:8>`.
	*/
	pub fn name(&self) -> Rc<str> {
		with_engine(|engine| {
			Rc::clone(&engine.syms.borrow()[self.0 as usize].name)
		})
	}

	/**
	Returns `true` if this symbol is a gensym.

	Gensyms are constructed using [`glsp::gensym`](fn.gensym.html) or
	[`glsp::gensym_with_tag`](fn.gensym_with_tag.html).
	*/
	pub fn is_gensym(&self) -> bool {
		self.kind() == SymKind::Gensym
	}

	//only public so that it can be used internally by the backquote!() macro
	#[doc(hidden)]
	pub fn kind(&self) -> SymKind {
		with_engine(|engine| {
			engine.syms.borrow()[self.0 as usize].kind
		})
	}

	//used internally by the encoder and by (bind-global!). we forbid special forms' names from 
	//being rebound to global or local variables, but not from being bound to a macro. a (let) 
	//macro has its uses, but a shadowing local variable named something like `defer` would break 
	//too many macros to be useful.
	pub(crate) fn is_bindable(&self) -> bool {
		match self.kind() {
			SymKind::StockSpecial => false,
			SymKind::Normal | SymKind::StockKeyword | 
			SymKind::StockTransform | SymKind::Gensym => true
		}
	}

	//only public so that it can be used internally by the backquote!() macro
	#[doc(hidden)]
	pub fn from_u32(id: u32) -> Sym {
		assert!(id <= MAX_SYM);
		Sym(id, PhantomData)
	}

	//only public so that it can be used internally by the backquote!() macro
	#[doc(hidden)]
	pub fn to_u32(&self) -> u32 {
		self.0
	}
}

impl PartialOrd<Sym> for Sym {
	fn partial_cmp(&self, other: &Sym) -> Option<Ordering> {
		if self.0 == other.0 {
			Some(Ordering::Equal)
		} else {
			(*self.name()).partial_cmp(&*other.name())
		}
	}
}

impl Ord for Sym {
	fn cmp(&self, other: &Sym) -> Ordering {
		if self.0 == other.0 {
			Ordering::Equal
		} else {
			(*self.name()).cmp(&*other.name())
		}
	}
}

/**
A type which can be converted to a [`Sym`](struct.Sym.html).

This is mostly used to make APIs more ergonomic. For example, the argument to 
[`glsp::global`](fn.global.html) is a generic `S: ToSym`, which means that it can 
receive either a symbol or a string:
	
	glsp::global(my_lib.my_sym)?;
	glsp::global("sym-name")?;
*/

pub trait ToSym {
	fn to_sym(&self) -> GResult<Sym>;
}

impl<'a, T> ToSym for T 
where
	T: Deref,
	T::Target: ToSym
{
	fn to_sym(&self) -> GResult<Sym> {
		(**self).to_sym()
	}
}

impl ToSym for Sym {
	fn to_sym(&self) -> GResult<Sym> {
		Ok(*self)
	}
}

impl<'a> ToSym for str {
	fn to_sym(&self) -> GResult<Sym> {
		glsp::sym(self)
	}
}

impl<'a> ToSym for Str {
	fn to_sym(&self) -> GResult<Sym> {
		glsp::sym(&self.to_string())
	}
}

#[doc(hidden)]
#[derive(Clone, Copy, PartialEq)]
pub enum SymKind {
	Normal,
	StockSpecial,
	StockKeyword,
	StockTransform,
	Gensym
}

/**
The `rfn` primitive type.

Rust functions are represented by a small `Copy` type (a 32-bit integer id).

Most of this type's methods belong to the `callable` abstract type, so they can be found in
the [`CallableOps`](trait.CallableOps.html) trait. To invoke an `RFn`, use
[`glsp::call`](fn.call.html).

To convert a function pointer or a closure into an `RFn`, you should usually call 
[`glsp::bind_rfn`](fn.bind_rfn.html) or [`glsp::rfn`](fn.rfn.html).
*/

//the PhantomData is used to ensure that RFns are !Send and !Sync

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct RFn(NonZeroU32, PhantomData<*mut ()>);

impl RFn {
	pub(crate) fn set_name(&self, new_name: Option<Sym>) {
		with_engine(|engine| {
			engine.rfns.borrow_mut()[self.0.get() as usize].name = new_name;
		})
	}
}

impl CallableOps for RFn {
	fn receive_call(&self, arg_count: usize) -> GResult<Val> {
		glsp::call_rfn(*self, arg_count).map(|slot| slot.root())
	}

	fn name(&self) -> Option<Sym> {
		with_engine(|engine| {
			engine.rfns.borrow()[self.0.get() as usize].name
		})
	}

	fn arg_limits(&self) -> (usize, Option<usize>) {
		with_engine(|engine| {
			engine.rfns.borrow()[self.0.get() as usize].wrapped_fn.arg_limits
		})
	}
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct Filename(NonZeroU32);

/**
Define a struct which contains a collection of symbols.

Caching symbols in a struct has better performance than calling [`glsp::sym`](fn.sym.html), and
it's more convenient than storing `Sym` fields in your own structs directly.

The struct defines a method `fn new() -> GResult<Self>` which initializes each field by passing
the given string literal to [`glsp::sym`](fn.sym.html).

	syms! {
		#[derive(Clone)]
		pub struct Syms {
			pub width: "width",
			pub set_width: "width=",
			rectp: "rect?"
		}
	}
*/

#[macro_export]
macro_rules! syms {
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $struct_name:ident {
			$($vis:vis $name:ident: $contents:literal),+
		}
	) => (
		$crate::syms! {
			$(#[$struct_attr])*
			$struct_vis struct $struct_name {
				$($vis $name: $contents,)+
			}
		}
	);

	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $struct_name:ident {
			$($vis:vis $name:ident: $contents:literal,)*
		}
	) => (
		$struct_vis struct $struct_name {
			$($vis $name: $crate::Sym,)*
		}

		impl $struct_name {
			#[allow(unused_variables)]
			$struct_vis fn new() -> $crate::GResult<Self> {
				Ok($struct_name {
					$($name: $crate::sym($contents)?,)*
				})
			}
		}
	);
}

/**
Constructs a symbol.

`sym!(arg)` is shorthand for [`glsp::sym(arg).unwrap()`](fn.sym.html).

This macro is convenient, but not necessarily efficient. To cache a large number of symbols
so that they don't have to be repeatedly recreated, use the [`syms!` macro](macro.syms.html).
*/

#[macro_export]
macro_rules! sym {
	($arg:expr,) => (sym!($arg));
	($arg:expr) => (
		$crate::sym($arg).unwrap()
	)
}

//-------------------------------------------------------------------------------------------------
// Lib, RData, RRoot
//-------------------------------------------------------------------------------------------------

/**
A type which can be passed to [`glsp::add_lib`](fn.add_lib.html).

It's possible to implement this trait manually, but using the [`lib!` macro](macro.lib.html) is
strongly encouraged. Among other things, that macro will automatically implement 
[`MakeArg`](trait.MakeArg.html) for the library type.
*/

pub trait Lib: 'static + Sized {
	fn type_name() -> &'static str;

	/**
	Returns a shared reference to the instance of this type owned by the active
	[`Runtime`](struct.Runtime.html).

	Panics if a library of this type has not been registered with the active `Runtime`, or if 
	the library is currently mutably borrowed.
	*/
	fn borrow() -> LibRef<Self> {
		glsp::lib::<Self>()
	}

	/**
	Returns a mutable reference to the instance of this type owned by the active
	[`Runtime`](struct.Runtime.html).

	Panics if a library of this type has not been registered with the active `Runtime`, or if 
	the library is currently borrowed.
	*/
	fn borrow_mut() -> LibRefMut<Self> {
		glsp::lib_mut::<Self>()
	}
	
	/**
	Returns a shared reference to the instance of this type owned by the active
	[`Runtime`](struct.Runtime.html).

	Returns an `Err` if a library of this type has not been registered with the active `Runtime`, 
	or if the library is currently mutably borrowed.
	*/
	fn try_borrow() -> GResult<LibRef<Self>> {
		glsp::try_lib::<Self>()
	}

	/**
	Returns a mutable reference to the instance of this type owned by the active
	[`Runtime`](struct.Runtime.html).

	Returns an `Err` if a library of this type has not been registered with the active `Runtime`, 
	or if the library is currently borrowed.
	*/
	fn try_borrow_mut() -> GResult<LibRefMut<Self>> {
		glsp::try_lib_mut::<Self>()
	}
}

/**
A reference to a [library](trait.Lib.html).

Created using [`Lib::borrow`](trait.Lib.html#method.borrow) or
[`Lib::try_borrow`](trait.Lib.html#method.try_borrow).
*/
pub struct LibRef<T: Lib> {
	handle: OwningHandle<Rc<RefCell<T>>, Ref<'static, T>>
}

impl<T: Lib> Deref for LibRef<T> {
	type Target = T;
	fn deref(&self) -> &T {
		&*self.handle
	}
}

/**
A mutable reference to a [library](trait.Lib.html).

Created using [`Lib::borrow_mut`](trait.Lib.html#method.borrow_mut) or
[`Lib::try_borrow_mut`](trait.Lib.html#method.try_borrow_mut).
*/
pub struct LibRefMut<T: Lib> {
	handle: OwningHandle<Rc<RefCell<T>>, RefMut<'static, T>>
}

impl<T: Lib> Deref for LibRefMut<T> {
	type Target = T;
	fn deref(&self) -> &T {
		&*self.handle
	}
}

impl<T: Lib> DerefMut for LibRefMut<T> {
	fn deref_mut(&mut self) -> &mut T {
		&mut *self.handle
	}
}

trait RAllocate: 'static {
	fn type_name(&self) -> &'static str;
	fn size_of(&self) -> usize;
	fn as_any(&self) -> &dyn Any;
	fn as_rc_any(self: Rc<Self>) -> Rc<dyn Any>;
}

impl<T: RStore> RAllocate for RefCell<T> {
	fn type_name(&self) -> &'static str {
		T::type_name()
	}

	fn size_of(&self) -> usize {
		T::size_of()
	}

	fn as_any(&self) -> &dyn Any {
		self
	}

	fn as_rc_any(self: Rc<Self>) -> Rc<dyn Any> {
		self
	}
}

/**
A type which can be moved onto the garbage-collected heap as an [`RData`](struct.RData.html).

It's possible to implement this trait manually, but using the [`rdata!` macro](macro.rdata.html) 
is strongly encouraged. Among other things, that macro will automatically implement 
[`MakeArg`](trait.MakeArg.html) and [`IntoResult`](trait.IntoResult.html) for your type.
*/

pub trait RStore: 'static {
	fn type_name() -> &'static str;

	//ideally we would provide a `fn owned_memory_usage(&self)` for this trait, but it's not clear
	//what the syntax would be for implementing it, or how we would document it. for now we just
	//report the size of the stored struct itself.
	fn size_of() -> usize;

	fn rclass() -> GResult<RClass>;
}

/**
An implementation detail of the [`RStore` trait](trait.RStore.html) and the
[`rdata!` macro](macro.rdata.html).
*/

pub struct RClass {
	name: Sym,
	bindings: FnvHashMap<Sym, RBinding>
}

enum RBinding {
	Met(RFn),
	Prop(Option<RFn>, Option<RFn>)
}

impl RClass {
	pub fn from_vec(
		class_name: &'static str,
		raw_bindings: Vec<(&'static str, &'static str, WrappedFn)>
	) -> GResult<RClass> {
		let class_name = glsp::sym(class_name)?;

		let mut bindings = FnvHashMap::with_capacity_and_hasher(
			raw_bindings.len(),
			Default::default()
		);

		for (kind, key, wrapped_fn) in raw_bindings {
			let name = glsp::sym(key)?;
			let rfn = glsp::rfn(wrapped_fn);
			if rfn.name().is_none() {
				rfn.set_name(Some(name));
			}

			match (kind, bindings.entry(name)) {
				("", Vacant(entry)) => { entry.insert(RBinding::Met(rfn)); }
				("get", Vacant(entry)) => { entry.insert(RBinding::Prop(Some(rfn), None)); }
				("set", Vacant(entry)) => { entry.insert(RBinding::Prop(None, Some(rfn))); }
				("", Occupied(_)) => bail!("duplicate method name {}", name),
				("get", Occupied(mut entry)) => {
					match entry.get_mut() {
						RBinding::Met(_) => {
							bail!("{} is bound to both a method and a property", name)
						}
						RBinding::Prop(Some(_), _) => bail!("duplicate getter {}", name),
						RBinding::Prop(ref mut none, _) => *none = Some(rfn)
					}
				}
				("set", Occupied(mut entry)) => {
					match entry.get_mut() {
						RBinding::Met(_) => {
							bail!("{} is bound to both a method and a property", name)
						}
						RBinding::Prop(_, Some(_)) => bail!("duplicate setter {}", name),
						RBinding::Prop(_, ref mut none) => *none = Some(rfn)
					}
				}
				(kind, _) => bail!("{} is not a valid tag for an RData method", kind)
			}
		}

		Ok(RClass {
			name: class_name,
			bindings
		})
	}
}

/**
The `rdata` primitive type.

This is a Rust value which has been moved onto the garbage-collected heap. It can be constructed
using the [`glsp::rdata`](fn.rdata.html) function, which returns a 
[`Root<RData>`](struct.Root.html).

`RData` has several convenience features:

- It supports interior mutability, like a 
  [`RefCell`](https://doc.rust-lang.org/std/cell/struct.RefCell.html).

- It can be manually deallocated, or taken back from the garbage-collected heap, using
  [`free`](#method.free) and [`take`](#method.take). Any attempts to access a deallocated `RData`
  from GameLisp will trigger an error.

- It's dynamically typed: a `Vec<Root<RData>>` could refer to several different Rust types.
  (If this is undesirable, consider using [`RRoot`](struct.RRoot.html) instead.)

- The [`rdata!` macro](macro.rdata.html) enables Rust functions to be associated with an `RData` 
  as its methods and properties. They can be accessed from GameLisp code, or accessed from Rust 
  code using an API similar to [`Obj`](struct.Obj.html).
*/

pub struct RData {
	header: GcHeader,
	storage: RefCell<Option<Rc<dyn RAllocate>>>,
	pub(crate) class: Rc<RClass>,

	//just like Obj, we need this field so that we can generate a `self` argument when 
	//rdata.call() is invoked from rust code
	gc_self: Cell<Option<Gc<RData>>>
}

impl Allocate for RData {
	fn header(&self) -> &GcHeader {
		&self.header
	}

	fn visit_gcs<V: Visitor>(&self, visitor: &mut V) {
		visitor.visit_gc(&self.gc_self());
	}

	fn clear_gcs(&self) {
		self.gc_self.set(None);
	}

	fn owned_memory_usage(&self) -> usize {
		match self.storage.borrow().as_ref() {
			Some(rc_ref) => rc_ref.size_of(),
			None => 0
		}
	}
}

/**
A shared reference to an [`RData`](struct.RData.html).

Created using [`RData::borrow`](struct.RData.html#method.borrow) or
[`RData::try_borrow`](struct.RData.html#method.try_borrow).
*/
pub struct RRef<T: RStore>(OwningHandle<Rc<RefCell<T>>, Ref<'static, T>>);

impl<T: RStore> Deref for RRef<T> {
	type Target = T;

	fn deref(&self) -> &T {
		&*self.0
	}
}

/**
A mutable reference to an [`RData`](struct.RData.html).

Created using [`RData::borrow_mut`](struct.RData.html#method.borrow_mut) or
[`RData::try_borrow_mut`](struct.RData.html#method.try_borrow_mut).
*/
pub struct RRefMut<T: RStore>(OwningHandle<Rc<RefCell<T>>, RefMut<'static, T>>);

impl<T: RStore> Deref for RRefMut<T> {
	type Target = T;

	fn deref(&self) -> &T {
		&*self.0
	}
}

impl<T: RStore> DerefMut for RRefMut<T> {
	fn deref_mut(&mut self) -> &mut T {
		&mut *self.0
	}
}

impl RData {
	pub(crate) fn new<T: RStore>(rdata: T, class: Rc<RClass>) -> RData {
		RData {
			header: GcHeader::new(),
			storage: RefCell::new(Some(Rc::new(RefCell::new(rdata)))),
			class,
			gc_self: Cell::new(None)
		}
	}

	pub fn type_name(&self) -> &'static str {
		match self.storage.borrow().as_ref() {
			Some(rc_ref) => rc_ref.type_name(),
			None => ""
		}
	}

	pub fn class_name(&self) -> Sym {
		self.class.name
	}

	/**
	Returns `true` if this `RData` is currently storing a value of type `T`.
	*/
	pub fn is<T: RStore>(&self) -> bool {
		self.storage.borrow().as_ref().unwrap().as_any().is::<RefCell<T>>()
	}

	/**
	Returns a shared reference to the value being stored by this `RData`.

	Panics if the `RData` is not storing a value of type `T`; if its value has been freed; or 
	if the value is currently mutably borrowed.
	*/
	pub fn borrow<T: RStore>(&self) -> RRef<T> {
		self.try_borrow::<T>().unwrap()
	}

	/**
	Returns a shared reference to the value being stored by this `RData`.

	Returns an `Err` if the `RData` is not storing a value of type `T`; if its value has 
	been freed; or if the value is currently mutably borrowed.
	*/
	pub fn try_borrow<T: RStore>(&self) -> GResult<RRef<T>> {
		let borrow = match self.storage.try_borrow() {
			Ok(borrow) => borrow,
			Err(_) => {
				//this is unreachable because the only time we *mutably* borrow the
				//outer RData is when we're about to free it
				unreachable!()
			}
		};

		if let Some(ref rc) = *borrow {
			match Rc::downcast::<RefCell<T>>(rc.clone().as_rc_any()) {
				Ok(rc_ref_cell) => {
					ensure!(rc_ref_cell.try_borrow().is_ok(),
					        "try_borrow<{}> failed: value is mutably borrowed", T::type_name());
					Ok(RRef(OwningHandle::new(rc_ref_cell)))
				}
				Err(_) => bail!("type mismatch in try_borrow<{}>()", T::type_name())
			}
		} else {
			bail!("try_borrow<{}> failed: attempted to access a freed RData", T::type_name())
		}
	}

	/**
	Returns a mutable reference to the value being stored by this `RData`.

	Panics if the `RData` is not storing a value of type `T`; if its value has been freed; or 
	if the value is currently borrowed.
	*/
	pub fn borrow_mut<T: RStore>(&self) -> RRefMut<T> {
		self.try_borrow_mut::<T>().unwrap()
	}

	/**
	Returns a mutable reference to the value being stored by this `RData`.

	Returns an `Err` if the `RData` is not storing a value of type `T`; if its value has 
	been freed; or if the value is currently borrowed.
	*/
	pub fn try_borrow_mut<T: RStore>(&self) -> GResult<RRefMut<T>> {
		let borrow = match self.storage.try_borrow() {
			Ok(borrow) => borrow,
			Err(_) => {
				//this is unreachable because the only time we *mutably* borrow the
				//outer RefCell is when we're about to free it
				unreachable!()
			}
		};

		if let Some(ref rc) = *borrow {
			match Rc::downcast::<RefCell<T>>(rc.clone().as_rc_any()) {
				Ok(rc_ref_cell) => {
					ensure!(rc_ref_cell.try_borrow_mut().is_ok(),
					        "try_borrow_mut<{}> failed: value is currently borrowed", 
					        T::type_name());
					Ok(RRefMut(OwningHandle::new_mut(rc_ref_cell)))
				}
				Err(_) => bail!("type mismatch in try_borrow<{}>()", T::type_name())
			}
		} else {
			bail!("try_borrow_mut<{}> failed: attempted to access a freed RData", T::type_name())
		}
	}

	/**
	Takes the value stored in this `RData` and returns it.

	Any future attempts to access the value will gracefully fail.

	Returns an `Err` if the `RData` is not storing a value of type `T`; if its value has already 
	been freed; or if it's currently borrowed.
	*/
	pub fn take<T: RStore>(&self) -> GResult<T> {
		//freeing the stored data changes our owned_memory_usage(), so we need to write-barrier it.
		let prev_usage = self.owned_memory_usage();

		match self.storage.try_borrow_mut() {
			Ok(mut borrow_mut) => {
				match *borrow_mut {
					Some(ref rc) if Rc::strong_count(rc) == 1 => {
						drop(rc);
						let rc_any = (*borrow_mut).take().unwrap().as_rc_any();
						drop(borrow_mut);

						let rc = match Rc::downcast::<RefCell<T>>(rc_any) {
							Ok(rc) => rc,
							Err(_) => {
								bail!("type mismatch when calling take::<{}>()", T::type_name())
							}
						};

						let cur_usage = self.owned_memory_usage();
						with_heap(|heap| heap.memory_usage_barrier(self, prev_usage, cur_usage));

						let ref_cell = Rc::try_unwrap(rc).ok().unwrap();
						Ok(ref_cell.into_inner())
					}
					Some(_) => bail!("called take() on an RData which is currently borrowed"),
					None => bail!("called take() on an RData which has already been freed")
				}
			}
			Err(_) => bail!("called take() on an RData which is currently borrowed")
		}
	}

	/**
	Drops the value stored by this `RData`.

	Any future attempts to access the value will gracefully fail.

	Returns an `Err` if the `RData` is currently borrowed, or if its value has already
	been freed.
	*/
	pub fn free(&self) -> GResult<()> {
		//freeing the stored data changes our owned_memory_usage(), so we need to write-barrier it.
		let prev_usage = self.owned_memory_usage();

		match self.storage.try_borrow_mut() {
			Ok(mut borrow_mut) => {
				match *borrow_mut {
					Some(ref rc) if Rc::strong_count(rc) == 1 => {
						drop(rc);
						*borrow_mut = None;
						drop(borrow_mut);

						let cur_usage = self.owned_memory_usage();
						with_heap(|heap| heap.memory_usage_barrier(self, prev_usage, cur_usage));

						Ok(())
					}
					Some(_) => bail!("called free() on an RData which is currently borrowed"),
					None => bail!("called free() on an RData which has already been freed")
				}
			}
			Err(_) => bail!("called take() on an RData which is currently borrowed")
		}
	}

	///Returns `true` if this `RData`'s value has been taken or freed.
	pub fn is_freed(&self) -> bool {
		match self.storage.try_borrow() {
			Ok(borrow) => borrow.is_none(),
			Err(_) => unreachable!()
		}
	}

	fn gc_self(&self) -> Gc<RData> {
		let gc_self = self.gc_self.take();
		self.gc_self.set(gc_self.clone());
		gc_self.unwrap()
	}

	/**
	Creates an indexing iterator for this collection.

	Equivalent to [`[rdata iter]`](https://gamelisp.rs/std/access).
	*/
	pub fn access_giter(rdata: &Root<RData>, giter: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::AccessRData(rdata.to_gc(), giter.to_gc()))
	}

	/**
	Accesses the value of a field or property.
	
	Equivalent to [`[rdata key]`](https://gamelisp.rs/std/access).
	*/
	pub fn get<S, R>(&self, key: S) -> GResult<R>
	where
		S: ToSym,
		R: FromVal
	{
		let sym = key.to_sym()?;
		match self.get_if_present(sym)? {
			Some(r) => Ok(r),
			None => bail!("attempted to access nonexistent prop getter '{}'", sym)
		}
	}

	/**
	Accesses the value of a field or property, if it exists.
	
	Equivalent to [`[rdata (? key)]`](https://gamelisp.rs/std/access).
	*/
	pub fn get_if_present<S, R>(&self, key: S) -> GResult<Option<R>>
	where
		S: ToSym,
		R: FromVal
	{
		let sym = key.to_sym()?;

		match self.class.bindings.get(&sym) {
			Some(RBinding::Prop(Some(rfn), _)) => {
				with_vm(|vm| {
					vm.stacks.borrow_mut().regs.push(Slot::RData(self.gc_self()));
					Ok(Some(R::from_val(&rfn.receive_call(1)?)?))
				})
			}
			_ => Ok(None)
		}
	}

	/**
	Mutates the field or property bound to the given name.
	
	Equivalent to [`(= [rdata key] val)`](https://gamelisp.rs/std/set-access).
	*/
	pub fn set<S, V>(&self, key: S, val: V) -> GResult<()>
	where
		S: ToSym,
		V: ToVal
	{
		let sym = key.to_sym()?;
		ensure!(self.set_if_present(sym, val)?, "attempted to assign to nonexistent or \
		        readonly prop '{}'", sym);
		Ok(())
	}

	/**
	Mutates the field or property bound to the given name, if any. Returns `true` if the
	field or property exists.
	
	Equivalent to [`(= [rdata (? key)] val)`](https://gamelisp.rs/std/set-access).
	*/
	pub fn set_if_present<S, V>(&self, key: S, val: V) -> GResult<bool>
	where
		S: ToSym,
		V: ToVal
	{
		let sym = key.to_sym()?;

		match self.class.bindings.get(&sym) {
			Some(RBinding::Prop(_, Some(rfn))) => {
				with_vm(|vm| {
					let mut stacks = vm.stacks.borrow_mut();
					stacks.regs.push(Slot::RData(self.gc_self()));
					stacks.regs.push(val.to_slot()?);
					drop(stacks);

					rfn.receive_call(2)?;
					Ok(true)
				})
			}
			_ => Ok(false)
		}
	}

	/**
	Invokes a method.
	
	Equivalent to [`(call-met rdata key ..args)`](https://gamelisp.rs/std/call-met).
	*/
	pub fn call<S, A, R>(&self, key: S, args: &A) -> GResult<R> 
	where
		S: ToSym,
		A: ToCallArgs + ?Sized, 
		R: FromVal
	{
		let sym = key.to_sym()?;
		match self.call_if_present(sym, args)? {
			Some(r) => Ok(r),
			None => bail!("attempted to call nonexistent method '{}'", sym)
		}
	}

	/**
	Invokes a method, if it exists.
	
	Equivalent to [`(call-met rdata (? key) ..args)`](https://gamelisp.rs/std/call-met).
	*/
	pub fn call_if_present<S, A, R>(&self, key: S, args: &A) -> GResult<Option<R>> 
	where
		S: ToSym,
		A: ToCallArgs + ?Sized, 
		R: FromVal
	{
		let sym = key.to_sym()?;

		match self.class.bindings.get(&sym) {
			Some(RBinding::Met(rfn)) => {
				with_vm(|vm| {
					let mut stacks = vm.stacks.borrow_mut();
					let starting_len = stacks.regs.len();

					stacks.regs.push(Slot::RData(self.gc_self()));
					args.to_call_args(&mut stacks.regs)?;

					let arg_count = stacks.regs.len() - starting_len;
					drop(stacks);

					let val = rfn.receive_call(arg_count)?;
					Ok(Some(R::from_val(&val)?))
				})
			}
			_ => Ok(None)
		}
	}

	/**
	Returns `true` if the given name is bound to a method.
	
	Equivalent to [`(has-met? rdata key)`](https://gamelisp.rs/std/has-met-p).
	*/
	pub fn has_met<S: ToSym>(&self, key: S) -> GResult<bool> {
		let sym = key.to_sym()?;

		match self.class.bindings.get(&sym) {
			Some(RBinding::Met(_)) => Ok(true),
			_ => Ok(false)
		}
	}

	//designed to imitate Obj::get_method(). used in vm.rs
	pub(crate) fn get_method(&self, key: Sym) -> Option<(Slot, bool, bool, Slot)> {
		match self.class.bindings.get(&key) {
			Some(&RBinding::Met(rfn)) => Some((Slot::RFn(rfn), true, false, Slot::Nil)),
			_ => None
		}
	}

	/**
	Equivalent to [`(eq? self other)`](https://gamelisp.rs/std/eq-p).

	Note that, because this may invoke an `op-eq?` method, it can potentially fail.

	The same is true for `PartialEq` comparisons between `RData` using Rust's `==` operator.
	In that case, if an error occurs, the operator will panic.
	*/
	pub fn try_eq(&self, other: &Root<RData>) -> GResult<bool> {
		if !Rc::ptr_eq(&self.class, &other.class) {
			return Ok(false)
		}

		let val: Option<Val> = self.call_if_present(OP_EQP_SYM, &[other])?;
		match val {
			Some(val) => Ok(val.is_truthy()),
			None => Ok(false)
		}
	}
}

impl PartialEq<Root<RData>> for RData {
	fn eq(&self, other: &Root<RData>) -> bool {
		self.try_eq(other).unwrap()
	}
}

/**
A strongly-typed reference to an [`RData`](struct.RData.html).

Equivalent to a [`Root<RData>`](struct.Root.html), but it enforces that the `RData` must contain
a value of type `T`.

`RRoot` tends to be more self-documenting than `Root<RData>`, and it has a slightly more
convenient API.
	
	//using Root
	let mesh = player_mesh.borrow::<Mesh>();
	let mesh2 = enemy_mesh.take::<Mesh>();

	//using RRoot
	let mesh = player_mesh.borrow();
	let mesh2 = enemy_mesh.take();
*/

pub struct RRoot<T: RStore>(Root<RData>, PhantomData<Rc<RefCell<T>>>);

impl<T: RStore> Clone for RRoot<T> {
	fn clone(&self) -> RRoot<T> {
		RRoot(self.0.clone(), PhantomData)
	}
}

impl<T: RStore> Debug for RRoot<T> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		Debug::fmt(&self.0, f)
	}
}

impl<T: RStore> Display for RRoot<T> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

impl<T: RStore> Pointer for RRoot<T> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		Pointer::fmt(&self.0, f)
	}
}

impl<T: RStore> RRoot<T> {
	/**
	Constructs an `RRoot<T>` from a `Root<RData>`.

	Panics if the `RData`'s value does not belong to the type `T`.
	*/
	pub fn new(root: Root<RData>) -> RRoot<T> {
		assert!(root.is::<T>(), "type mismatch when constructing an RRoot<{}>", T::type_name());
		RRoot(root, PhantomData)
	}

	///Returns a copy of the underlying `Root<RData>`.
	pub fn to_root(&self) -> Root<RData> {
		self.0.clone()
	}

	///Drops the `RRoot`, returning the wrapped `Root<RData>`.
	pub fn into_root(self) -> Root<RData> {
		self.0
	}

	///Returns `true` if both `RRoots` refer to the same `RData`.
	pub fn ptr_eq(rr0: &RRoot<T>, rr1: &RRoot<T>) -> bool {
		Root::ptr_eq(&rr0.0, &rr1.0)
	}

	pub(crate) fn to_gc(&self) -> Gc<RData> {
		self.0.to_gc()
	}

	#[allow(dead_code)]
	pub(crate) fn into_gc(self) -> Gc<RData> {
		self.0.into_gc()
	}

	///Equivalent to [`RData::borrow`](struct.RData.html#method.borrow).
	pub fn borrow(&self) -> RRef<T> {
		self.0.borrow()
	}

	///Equivalent to [`RData::borrow_mut`](struct.RData.html#method.borrow_mut).
	pub fn borrow_mut(&self) -> RRefMut<T> {
		self.0.borrow_mut()
	}

	///Equivalent to [`RData::try_borrow`](struct.RData.html#method.try_borrow).
	pub fn try_borrow(&self) -> GResult<RRef<T>> {
		self.0.try_borrow()
	}

	///Equivalent to [`RData::try_borrow_mut`](struct.RData.html#method.try_borrow_mut).
	pub fn try_borrow_mut(&self) -> GResult<RRefMut<T>> {
		self.0.try_borrow_mut()
	}

	///Equivalent to [`RData::take`](struct.RData.html#method.take).
	pub fn take(&self) -> GResult<T> {
		self.0.take()
	}

	///Equivalent to [`RData::free`](struct.RData.html#method.free).
	pub fn free(&self) -> GResult<()> {
		self.0.free()
	}

	///Equivalent to [`RData::is_freed`](struct.RData.html#method.is_freed).
	pub fn is_freed(&self) -> bool {
		self.0.is_freed()
	}
}


//-------------------------------------------------------------------------------------------------
// Span, SpanStorage and Frame
//-------------------------------------------------------------------------------------------------

//all arrs are tagged with a Span, indicating where they were created. we use this information 
//for reporting line numbers when an error occurs.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum SpanStorage {

	//this arr was parsed from the text of a file which was passed to (load) or glsp.load(). 
	//the Filename identifies the file, and the usize is the 1-indexed line number.
	Loaded(Filename, usize),

	//this arr was allocated within a macro expander. the Sym is the macro's name, the first Span 
	//identifies the macro's callsite (i.e. the Span of the arr from which it was expanded), and
	//the second Span identifies the form's construction site (i.e. the Span of the (arr) or
	//(clone) call, within a macro expander, which created the form).
	Expanded(Option<Sym>, Span, Span),

	//a broad category for any other arr. in practice, arrs which aren't loaded from a file or
	//created within a macro expander are very unlikely to be used as syntax. when they are,
	//the user is unlikely to care exactly where they were generated.
	Generated
}

#[doc(hidden)]
#[derive(Default, PartialEq, Eq, Hash, Copy, Clone)]
pub struct Span(u32);


//-------------------------------------------------------------------------------------------------
// glsp:: functions
//-------------------------------------------------------------------------------------------------

pub mod glsp {
	use super::*;

	//---------------------------------------------------------------------------------------------
	// syms
	//---------------------------------------------------------------------------------------------

	/** Equivalent to [`(sym name)`](https://gamelisp.rs/std/sym). */

	pub fn sym(name: &str) -> GResult<Sym> {
		ensure!(glsp::is_valid_sym(name), "invalid sym '{}'", name);
		glsp::sym_impl(name, SymKind::Normal)
	}

	fn sym_impl(name: &str, kind: SymKind) -> GResult<Sym> {
		with_engine(|engine| {
			let mut syms_map =  engine.syms_map.borrow_mut();
			if let Some(sym) = syms_map.get(name) {
				Ok(*sym)
			} else {
				let name = Rc::<str>::from(name);

				let mut syms = engine.syms.borrow_mut();
				syms.push(SymEntry {
					name: name.clone(),
					kind,
					bound_global: None,
					bound_macro: None
				});
				
				//we panic rather than returning an Err here, becuase we consider running out of
				//Syms to be an unrecoverable error, similar to out-of-memory in a Rust program
				assert!(syms.len() - 1 <= MAX_SYM as usize,
				        "program requires more than {} unique symbols", MAX_SYM + 1);
				
				let sym = Sym((syms.len() - 1) as u32, PhantomData);
				syms_map.insert(name, sym);
				
				Ok(sym)
			}
		})
	}

	/** Equivalent to [`(valid-sym-str? st)`](https://gamelisp.rs/std/valid-sym-str-p). */

	pub fn is_valid_sym(st: &str) -> bool {
		//one or more of the valid sym chars, optionally with a '#' suffix
		let mut rev_iter = st.chars().rev();
		match rev_iter.next() {
			None => false,
			Some('#') => st.len() > 1 && rev_iter.all(|ch| lex::is_valid_sym_char(ch)),
			Some(last_char) => {
				lex::is_valid_sym_char(last_char) && rev_iter.all(|ch| {
					lex::is_valid_sym_char(ch)
				})
			}
		}
	}

	/** Equivalent to [`(valid-sym-char? ch)`](https://gamelisp.rs/std/valid-sym-char-p). */

	pub fn is_valid_sym_char(ch: char) -> bool {
		lex::is_valid_sym_char(ch)
	}

	/** 
	Equivalent to [`(representable-sym-str? st)`](https://gamelisp.rs/std/representable-sym-str-p).
	*/

	pub fn is_representable_sym(st: &str) -> bool {
		if !is_valid_sym(st) {
			false
		} else {
			sym(st).unwrap().is_representable()
		}
	}

	/** Equivalent to [`(gensym)`](https://gamelisp.rs/std/gensym). */

	pub fn gensym() -> Sym {
		glsp::gensym_impl(None)
	}

	/** Equivalent to [`(gensym tag)`](https://gamelisp.rs/std/gensym). */

	pub fn gensym_with_tag(tag: &str) -> GResult<Sym> {
		ensure!(glsp::is_valid_sym(tag) && !tag.ends_with("#"),
		        "invalid gensym tag '{}': tags should be a valid sym without a trailing '#'",
		        tag);
		Ok(glsp::gensym_impl(Some(tag)))
	}

	fn gensym_impl(tag: Option<&str>) -> Sym {
		with_engine(|engine| {
			let counter = engine.gensym_counter.get();
			let seed = engine.gensym_seed.borrow();

			let mut bytes = SmallVec::<[u8; 64]>::new();
			write!(&mut bytes, "#<gs").unwrap();
			if let Some(tag) = tag {
				write!(&mut bytes, ":{}", tag).unwrap();
			}
			write!(&mut bytes, ":{}", counter).unwrap();
			if let Some(ref seed) = *seed {
				write!(&mut bytes, ":{}", seed).unwrap();
			}
			write!(&mut bytes, ">").unwrap();

			engine.gensym_counter.set(counter + 1);
			drop(seed);

			let name = str::from_utf8(&bytes[..]).unwrap();
			glsp::sym_impl(name, SymKind::Gensym).unwrap()
		})
	}

	/**
	Makes future gensyms globally unique.

	When code is expanded in one Runtime and then evaluated in another (as for the 
	[`compile!`](macro.compile.html) and [`eval!`](macro.eval.html) macros, and the
	[`glsp::load_and_compile`](fn.load_and_compile.html) function), gensym collisions may occur,
	because the gensym counter will have been reset.

	`glsp::seed_gensym` mitigates this by appending a 128-bit ID (derived from the current
	wall-clock time) to all future gensyms produced by this `Runtime`. We don't switch it on by
	default, because it makes gensyms' printed representation harder to read. However, it's
	automatically switched on within [`glsp::load_and_compile`](fn.load_and_compile.html).

		prn!("{}", glsp::gensym()); //prints #<gs:0>
		glsp::seed_gensym();
		prn!("{}", glsp::gensym()); //prints #<gs:1:wTz8iriBJYB>
	*/

	pub fn seed_gensym() {
		with_engine(|engine| {
			//this seems to have at least 100ns resolution
			let nanos = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();

			//to keep it as concise as possible, we use base64
			static CHARS: [char; 64] = [
				'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 
				'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 
				'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 
				'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
				'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '_'
			];

			//this reverses the order of the characters, but it doesn't matter
			let mut seed = String::new();
			let mut bits = nanos;
			while bits > 0 {
				seed.push(CHARS[(bits & 0x3f) as usize]);
				bits >>= 6;
			}

			*engine.gensym_seed.borrow_mut() = Some(seed);
		})
	}

	//---------------------------------------------------------------------------------------------
	// globals
	//---------------------------------------------------------------------------------------------

	/** Equivalent to [`(global s)`](https://gamelisp.rs/std/global). */

	pub fn global<S, T>(s: S) -> GResult<T> 
	where
		S: ToSym, 
		T: FromVal
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;

			let syms = engine.syms.borrow();
			let entry = &syms[sym.0 as usize];
			match entry.bound_global {
				Some(ref global) => T::from_val(&global.val),
				None => bail!("symbol {} is not bound to a global", entry.name)
			}
		})
	}

	pub(crate) fn try_global<S, T>(s: S) -> GResult<Option<T>> 
	where
		S: ToSym, 
		T: FromVal
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;
			match engine.syms.borrow()[sym.0 as usize].bound_global {
				Some(ref global) => T::from_val(&global.val).map(|t| Some(t)),
				None => Ok(None)
			}
		})
	}

	/** Equivalent to [`(= (global s) val)`](https://gamelisp.rs/std/set-global). */

	pub fn set_global<S, T>(s: S, val: T) -> GResult<()>
	where
		S: ToSym,
		T: ToVal
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;
			let val = val.to_val()?;

			let mut syms = engine.syms.borrow_mut();
			let entry = &mut syms[sym.0 as usize];

			match entry.bound_global {
				Some(ref mut global) => {
					if global.frozen {
						let name = entry.name.clone();
						drop(global);
						drop(entry);
						drop(syms);
						bail!("attempted to mutate frozen global {}", name);
					}

					global.val = val;
					Ok(())
				}
				None => {
					let name = entry.name.clone();
					drop(entry);
					drop(syms);
					bail!("symbol {} is not bound to a global", name)
				}
			}
		})
	}

	pub(crate) enum TrySetGlobalOutcome {
		Success,
		NotBound,
		Frozen
	}

	pub(crate) fn try_set_global<S, T>(s: S, t: T) -> GResult<TrySetGlobalOutcome>
	where
		S: ToSym, 
		T: ToVal
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;
			let val = t.to_val()?;

			let mut syms = engine.syms.borrow_mut();
			let entry = &mut syms[sym.0 as usize];

			match entry.bound_global {
				Some(ref mut global) => {
					if global.frozen {
						return Ok(TrySetGlobalOutcome::Frozen)
					}

					global.val = val;
					Ok(TrySetGlobalOutcome::Success)
				}
				None => Ok(TrySetGlobalOutcome::NotBound)
			}
		})
	}

	/** Equivalent to [`(freeze-global! s)`](https://gamelisp.rs/std/freeze-global-mut). */

	pub fn freeze_global<S>(s: S) -> GResult<()>
	where
		S: ToSym
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;

			let mut syms = engine.syms.borrow_mut();
			let entry = &mut syms[sym.0 as usize];

			match entry.bound_global {
				Some(ref mut global) => {
					global.frozen = true;
					Ok(())
				}
				None => {
					let name = entry.name.clone();
					drop(entry);
					drop(syms);
					bail!("attempted to freeze the unbound global {}", name)
				}
			}
		})
	}

	//freezes each global which might be caught by the op-transforms pass, like "+". we can't do
	//this in the Engine constructor because the stdlib initialization code needs to be able to
	//bind new rfns to these syms.
	#[doc(hidden)]
	pub fn freeze_transform_fns() {
		with_engine(|engine| {
			let mut syms = engine.syms.borrow_mut();

			for sym in STOCK_SYMS_BY_KIND[2] {
				let entry = &mut syms[sym.0 as usize];

				if entry.kind != SymKind::StockTransform {
					panic!()
				}

				if let Some(ref mut global) = entry.bound_global {
					global.frozen = true;
				} else {
					panic!("transform fn {} is not bound", entry.name)
				}
			}
		})
	}

	/** Equivalent to [`(has-global? s)`](https://gamelisp.rs/std/has-global-p). */

	pub fn has_global<S>(s: S) -> GResult<bool>
	where
		S: ToSym
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;
			Ok(engine.syms.borrow()[sym.0 as usize].bound_global.is_some())
		})
	}

	/** Equivalent to [`(bind-global! s)`](https://gamelisp.rs/std/bind-global-mut). */

	pub fn bind_global<S, T>(s: S, t: T) -> GResult<()>
	where
		S: ToSym, 
		T: ToVal
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;
			ensure!(sym.is_bindable(), "unable to bind name '{}' to global", sym);

			let val = t.to_val()?;

			let mut syms = engine.syms.borrow_mut();
			let entry = &mut syms[sym.0 as usize];

			if entry.bound_global.is_none() {
				entry.bound_global = Some(GlobalEntry {
					val,
					frozen: false
				});

				Ok(())
			} else {
				let name = entry.name.clone();
				drop(entry);
				drop(syms);
				bail!("attempted to bind the global '{}', which is already bound", name)
			}
		})
	}

	/** Equivalent to [`(del-global! s)`](https://gamelisp.rs/std/del-global-mut). */

	pub fn del_global<S>(s: S) -> GResult<()>
	where
		S: ToSym
	{
		with_engine(|engine| {
			let sym = s.to_sym()?;

			let mut syms = engine.syms.borrow_mut();
			let entry = &mut syms[sym.0 as usize];

			if let Some(ref mut global) = entry.bound_global {
				ensure!(!global.frozen, "attempted to unbind '{}', which is frozen", entry.name);

				entry.bound_global = None;
				Ok(())
			} else {
				let name = entry.name.clone();
				drop(entry);
				drop(syms);
				bail!("attempted to unbind '{}', which is not bound to a global", name)
			}
		})
	}

	//---------------------------------------------------------------------------------------------
	// macros
	//---------------------------------------------------------------------------------------------

	/** Equivalent to [`(macro s)`](https://gamelisp.rs/std/macro). */

	pub fn get_macro<S: ToSym>(s: S) -> GResult<Expander> {
		with_engine(|engine| {
			let sym = s.to_sym()?;
			match engine.syms.borrow()[sym.0 as usize].bound_macro {
				Some(ref expander) => Ok(expander.clone()),
				None => bail!("symbol {} is not bound to a macro", sym)
			}
		})
	}

	/** Equivalent to [`(= (macro s) expander)`](https://gamelisp.rs/std/set-macro). */

	pub fn set_macro<S: ToSym>(s: S, expander: Expander) -> GResult<()> {
		with_engine(|engine| {
			let sym = s.to_sym()?;
			let mut syms = engine.syms.borrow_mut();
			match syms[sym.0 as usize].bound_macro {
				Some(ref mut storage) => {
					*storage = expander;
					Ok(())
				}
				None => {
					drop(syms);
					bail!("symbol {} is not bound to a macro", sym)
				}
			}
		})
	}

	/** Equivalent to [`(has-macro? s)`](https://gamelisp.rs/std/has-macro-p). */

	pub fn has_macro<S: ToSym>(s: S) -> GResult<bool> {
		with_engine(|engine| {
			let sym = s.to_sym()?;
			Ok(engine.syms.borrow()[sym.0 as usize].bound_macro.is_some())
		})
	}

	/** Equivalent to [`(bind-macro! s)`](https://gamelisp.rs/std/bind-macro-mut). */

	pub fn bind_macro<S: ToSym>(s: S, expander: Expander) -> GResult<()> {
		with_engine(|engine| {
			let sym = s.to_sym()?;
			let mut syms = engine.syms.borrow_mut();
			match syms[sym.0 as usize].bound_macro {
				Some(_) => {
					drop(syms);
					bail!("attempted to bind the macro {}, which is already bound", sym)
				}
				ref mut storage @ None => {
					*storage = Some(expander);
					Ok(())
				}
			}
		})
	}

	/** Equivalent to [`(del-macro! s)`](https://gamelisp.rs/std/del-macro-mut). */

	pub fn del_macro<S: ToSym>(s: S) -> GResult<()> {
		with_engine(|engine| {
			let sym = s.to_sym()?;
			let mut syms = engine.syms.borrow_mut();
			match syms[sym.0 as usize].bound_macro {
				ref mut storage @ Some(_) => {
					*storage = None;
					Ok(())
				}
				None => {
					drop(syms);
					bail!("attempted to unbind the macro {}, which is not bound", sym)
				}
			}
		})
	}

	//---------------------------------------------------------------------------------------------
	// filenames
	//---------------------------------------------------------------------------------------------

	#[doc(hidden)]
	pub fn filename(st: &str) -> Filename {
		with_engine(|engine| {
			let mut filenames_map = engine.filenames_map.borrow_mut();
			if let Some(filename) = filenames_map.get(st) {
				*filename
			} else {
				let mut filenames = engine.filenames.borrow_mut();

				//see glsp::sym() for the rationale behind panicking here
				assert!(filenames.len() <= u32::MAX as usize,
				        "cannot load more than {} unique files", (u32::MAX as u64) + 1);

				let rc = Rc::<str>::from(st);
				filenames.push(Rc::clone(&rc));

				let id = u32::try_from(filenames.len() - 1).unwrap();
				let filename = Filename(NonZeroU32::new(id).unwrap());
				filenames_map.insert(rc, filename);
				
				filename
			}
		})
	}

	pub(crate) fn filename_str(filename: Filename) -> Rc<str> {
		with_engine(|engine| {
			Rc::clone(&engine.filenames.borrow()[filename.0.get() as usize])
		})
	}

	//---------------------------------------------------------------------------------------------
	// libs, rdata
	//---------------------------------------------------------------------------------------------

	/**
	Moves a Rust value onto the garbage-collected heap.

	Rust types are automatically registered with the `Runtime` when `glsp::rdata` is first called
	for a value of that type. If two types share the same unprefixed name, like
	`video::Clip` and `audio::Clip`, it's an error.
	*/

	pub fn rdata<T: RStore>(rdata: T) -> GResult<Root<RData>> {
		with_engine(|engine| {
			let class_rc = match engine.rclasses.borrow_mut().entry(rdata.type_id()) {
				Vacant(entry) => {
					let type_name = T::type_name();
					ensure!(engine.rclass_names.borrow_mut().insert(&type_name),
					        "multiple distinct RData types share the same name: {}", type_name);

					let class = Rc::new(T::rclass()?);
					entry.insert(Rc::clone(&class));
					class
				}
				Occupied(entry) => {
					Rc::clone(&*entry.get())
				}
			};

			let root = glsp::alloc(RData::new(rdata, class_rc));
			root.gc_self.set(Some(root.to_gc()));
			Ok(root)
		})
	}

	/**
	Moves a Rust value onto the garbage-collected heap, returning a typed pointer.

	This function is a shorthand for
	[`RRoot::<T>::new(glsp::rdata(rdata)?)`](struct.RRoot.html#method.new).
	*/
	pub fn rroot<T: RStore>(rdata: T) -> GResult<RRoot<T>> {
		Ok(RRoot::new(glsp::rdata(rdata)?))
	}

	/**
	Registers an instance of a library type.

	Library types are singletons: if the active `Runtime` already contains a library of type
	`T`, this function will panic.

	When the active `Runtime` is dropped, each of its libraries will be dropped in the
	reverse order that they were registered.

	Once registered, it's possible to remove a library from the `Runtime` with
	[`glsp::take_lib`](fn.take_lib.html), or borrow it with [`glsp::lib`](fn.lib.html), 
	[`glsp::lib_mut`](fn.lib_mut.html), [`glsp::try_lib`](fn.try_lib.html) and
	[`glsp::try_lib_mut`](fn.try_lib_mut.html).

	It can be more convenient to borrow a library by calling 
	[`T::borrow()`](trait.Lib.html#method.borrow), and other similar methods on the
	[`Lib` trait](trait.Lib.html).
	*/

	pub fn add_lib<T: Lib>(lib: T) {
		with_engine(|engine| {
			let rc = Rc::new(RefCell::new(lib)) as Rc<dyn Any>;
			if engine.libs.borrow_mut().insert(TypeId::of::<T>(), rc).is_some() {
				panic!("glsp.add_lib() called twice for the same type");
			}

			engine.libs_ordering.borrow_mut().push(TypeId::of::<T>());
		})
	}

	/**
	Unregisters a value previously registered using [`glsp::add_lib`](fn.add_lib.html), and
	returns it.

	Returns `Err` if no library is registered for the type `T`, or if a library exists but it's
	currently borrowed.
	*/

	pub fn take_lib<T: Lib>() -> GResult<T> {
		with_engine(|engine| {
			let rc = match engine.libs.borrow_mut().remove(&TypeId::of::<T>()) {
				Some(rc) => rc.downcast::<RefCell<T>>().unwrap(),
				None => bail!("attempted to take nonexistent lib {}", type_name::<T>())
			};

			let lib = match Rc::try_unwrap(rc) {
				Ok(ref_cell) => ref_cell.into_inner(),
				Err(_) => bail!("called take_lib for {}, which is currently borrowed", 
				                type_name::<T>())
			};

			engine.libs_ordering.borrow_mut().retain(|&type_id| type_id != TypeId::of::<T>());

			Ok(lib)
		})
	}

	///Equivalent to [`Lib::borrow`](trait.Lib.html#method.borrow).
	pub fn lib<T: Lib>() -> LibRef<T> {
		match glsp::try_lib::<T>() {
			Ok(lib_ref) => lib_ref,
			Err(err) => panic!("{}", err.val())
		}
	}

	///Equivalent to [`Lib::borrow_mut`](trait.Lib.html#method.borrow_mut).
	pub fn lib_mut<T: Lib>() -> LibRefMut<T> {
		match glsp::try_lib_mut::<T>() {
			Ok(lib_ref_mut) => lib_ref_mut,
			Err(err) => panic!("{}", err.val())
		}
	}

	///Equivalent to [`Lib::try_borrow`](trait.Lib.html#method.try_borrow).
	pub fn try_lib<T: Lib>() -> GResult<LibRef<T>> {
		with_engine(|engine| {
			let libs = engine.libs.borrow();

			let rc = match libs.get(&TypeId::of::<T>()) {
				Some(rc) => rc.clone(),
				None => bail!("lib type {} was never registered, or has been \
				               dropped", type_name::<T>())
			};

			let rc_ref_cell = rc.downcast::<RefCell<T>>().unwrap();

			if rc_ref_cell.try_borrow().is_err() {
				bail!("attempted to borrow lib {} during a mutable borrow", type_name::<T>())
			}

			Ok(LibRef {
				handle: OwningHandle::new(rc_ref_cell)
			})
		})
	}

	///Equivalent to [`Lib::try_borrow_mut`](trait.Lib.html#method.try_borrow_mut).
	pub fn try_lib_mut<T: Lib>() -> GResult<LibRefMut<T>> {
		with_engine(|engine| {
			let libs = engine.libs.borrow();

			let rc = match libs.get(&TypeId::of::<T>()) {
				Some(rc) => rc.clone(),
				None => bail!("lib type {} was never registered, or has been \
				               dropped", type_name::<T>())
			};

			let rc_ref_cell = rc.downcast::<RefCell<T>>().unwrap();

			if rc_ref_cell.try_borrow_mut().is_err() {
				bail!("attempted to mutably borrow {}, which is already borrowed", type_name::<T>())
			}

			Ok(LibRefMut {
				handle: OwningHandle::new_mut(rc_ref_cell)
			})
		})
	}

	//---------------------------------------------------------------------------------------------
	// rfns
	//---------------------------------------------------------------------------------------------

	/**
	Creates a GameLisp value which represents a Rust function.

	The `wrapped_fn` parameter should be constructed using the [`rfn!()`](macro.rfn.html) macro.

	When binding a Rust function to a global variable, it's usually more convenient to use 
	[`glsp::bind_rfn`](fn.bind_rfn.html) instead.

	Functions at the same address are deduplicated. For example, repeated calls to 
	`glsp::rfn("swap-bytes", i32::swap_bytes)` will return the same `RFn` every time.
	*/

	pub fn rfn(wrapped_fn: WrappedFn) -> RFn {
		with_engine(|engine| {
			let address = wrapped_fn.as_usize();

			let mut rfns_map =  engine.rfns_map.borrow_mut();
			if let Some(rfn) = rfns_map.get(&address) {
				*rfn
			} else {
				let mut rfns = engine.rfns.borrow_mut();
				rfns.push(RFnEntry {
					name: None,
					wrapped_fn
				});

				let id = u32::try_from(rfns.len() - 1).unwrap();
				let rfn = RFn(NonZeroU32::new(id).unwrap(), PhantomData);
				rfns_map.insert(address, rfn);

				rfn
			}
		})
	}

	/**
	Creates a GameLisp value which represents a Rust function, with a name.

	This is equivalent to [`glsp::rfn`](fn.rfn.html), but 
	[`(fn-name rfn)`](https://gamelisp.rs/std/fn-name) will return the given symbol, 
	rather than returning `#n`.
	*/

	pub fn named_rfn(name: Sym, wrapped_fn: WrappedFn) -> RFn {
		let rfn = glsp::rfn(wrapped_fn);
		rfn.set_name(Some(name));
		rfn
	}

	/**
	Binds a Rust function to a global variable.

	`glsp::bind_rfn(name, rfn!(f))?` is equivalent to:

		let sym = name.to_sym()?
		let rfn = glsp::named_rfn(sym, rfn!(f));
		glsp::bind_global(sym, rfn)?;
		Ok(rfn)
	*/

	pub fn bind_rfn<S: ToSym>(name: S, wrapped_fn: WrappedFn) -> GResult<RFn> {
		let sym = name.to_sym()?;

		let rfn = glsp::named_rfn(sym, wrapped_fn);

		glsp::bind_global(sym, rfn)?;
		Ok(rfn)
	}

	/**
	Binds a Rust function to a global macro.

	`glsp::bind_rfn_macro(name, rfn!(f))?` is equivalent to:

		let sym = name.to_sym()?
		let rfn = glsp::named_rfn(sym, rfn!(f));
		glsp::bind_macro(sym, Expander::RFn(rfn))?;
		Ok(rfn)
	*/

	pub fn bind_rfn_macro<S: ToSym>(name: S, wrapped_fn: WrappedFn) -> GResult<RFn> {
		let sym = name.to_sym()?;

		let rfn = glsp::named_rfn(sym, wrapped_fn);
		
		glsp::bind_macro(sym, Expander::RFn(rfn))?;
		Ok(rfn)
	}

	pub(crate) fn call_rfn(rfn: RFn, arg_count: usize) -> GResult<Slot> {
		with_engine(|engine| {

			/*
			when invoking a wrapped rfn, we borrow the vm's reg stack, copy the useful parts of
			it to the Rust callstack (as the Temps type), drop the borrow, and then invoke the 
			rfn. we only pop the regs after the call returns, so that they remain rooted.
			*/

			let stacks = engine.vm.stacks.borrow();
			let base_reg = stacks.regs.len() - arg_count;

			let _guard = Guard::new(|| {
				let mut stacks = engine.vm.stacks.borrow_mut();
				stacks.regs.truncate(base_reg);
			});

			let regs = Ref::map(stacks, |stacks| &stacks.regs[base_reg..]);

			let wrapped_fn = engine.rfns.borrow()[rfn.0.get() as usize].wrapped_fn;

			let result = panic::catch_unwind(AssertUnwindSafe(|| {
				wrapped_fn.call(regs)
			}));

			/*
			for the time being, we don't go through the rigmarole of trying to set a custom panic
			hook. it's a global resource, and managing that would be annoying. instead, we allow
			the normal panic hook to print its usual message, and we convert the caught panic
			into a generic message without any details.
			*/
			
			match result {
				Ok(glsp_result) => glsp_result,
				Err(payload) => {
					let rfn_description = match rfn.name() {
						Some(sym) => format!("rfn ({})", sym),
						None => format!("anonymous rfn")
					};

					if let Some(msg) = payload.downcast_ref::<&str>() {
						bail!("{} panicked, '{}'", rfn_description, msg)
					} else if let Some(msg) = payload.downcast_ref::<String>() {
						bail!("{} panicked, '{}'", rfn_description, msg)
					} else {
						bail!("{} panicked", rfn_description)
					}
				}
			}
		})
	}

	//---------------------------------------------------------------------------------------------
	// parsing and printing
	//---------------------------------------------------------------------------------------------

	/** Equivalent to [`(parse text filename)`](https://gamelisp.rs/std/parse). */

	pub fn parse(text: &mut &str, filename: Option<&str>) -> GResult<Option<Val>> {
		let file_id = filename.map(|path| glsp::filename(path));

		let mut parser = Parser::new(file_id);

		glsp::push_frame(Frame::GlspApi(GlspApiName::Parse, file_id));
		let _guard = Guard::new(|| glsp::pop_frame());

		while text.len() > 0 {
			if let Some(form) = parser.parse(text)? {
				return Ok(Some(form))
			}
		}

		parser.ensure_finished()?;
		Ok(None)
	}

	/** Equivalent to [`(parse-all text filename)`](https://gamelisp.rs/std/parse-all). */

	pub fn parse_all(mut text: &str, filename: Option<&str>) -> GResult<Vec<Val>> {
		let file_id = filename.map(|path| glsp::filename(path));

		glsp::push_frame(Frame::GlspApi(GlspApiName::ParseAll, file_id));
		let _guard = Guard::new(|| glsp::pop_frame());

		let mut parser = Parser::new(file_id);
		let mut results = Vec::new();
		while text.len() > 0 {
			if let Some(form) = parser.parse(&mut text)? {
				results.push(form);
			}
		}

		parser.ensure_finished()?;
		Ok(results)
	}

	/** Equivalent to [`(parse-1 text filename)`](https://gamelisp.rs/std/parse-1). */

	pub fn parse_1(mut text: &str, filename: Option<&str>) -> GResult<Val> {
		let file_id = filename.map(|path| glsp::filename(path));

		glsp::push_frame(Frame::GlspApi(GlspApiName::Parse1, file_id));
		let _guard = Guard::new(|| glsp::pop_frame());

		let mut parser = Parser::new(file_id);
		while text.len() > 0 {
			if let Some(form) = parser.parse(&mut text)? {
				while text.len() > 0 {
					if let Some(_) = parser.parse(&mut text)? {
						bail!("parse-1 produced multiple forms")
					}
				}

				return Ok(form)
			}
		}

		bail!("parse-1 did not produce a form")
	}

	/**
	Changes the output writer used by [`pr`](https://gamelisp.rs/std/pr),
	[`prn`](https://gamelisp.rs/std/prn), [`pr!`](macro.pr.html) and 
	[`prn!`](macro.prn.html).

	The default writer is [`Stdout`](https://doc.rust-lang.org/std/io/struct.Stdout.html). Note
	that calling `glsp::set_pr_writer` will not redirect the output of Rust macros like
	[`println!`](https://doc.rust-lang.org/std/macro.println.html): for that, you would need
	to use the undocumented feature [`set_print`](https://github.com/rust-lang/rust/issues/31343).

		//silences pr!(), prn!(), pr and prn
		glsp::set_pr_writer(Box::new(std::io::sink()));
	*/

	pub fn set_pr_writer(pr_writer: Box<dyn Write>) {
		with_engine(|engine| {
			*engine.pr_writer.borrow_mut() = pr_writer;
		})
	}

	/**
	Changes the output writer used by [`epr`](https://gamelisp.rs/std/epr),
	[`eprn`](https://gamelisp.rs/std/eprn), [`epr!`](macro.epr.html) and 
	[`eprn!`](macro.eprn.html).

	That same writer is also used by GameLisp to print a stack trace when an error is 
	not caught.

	The default writer is [`Stderr`](https://doc.rust-lang.org/std/io/struct.Stderr.html). Note
	that calling `glsp::set_epr_writer` will not redirect the output of Rust macros like
	[`panic!`](https://doc.rust-lang.org/std/macro.panic.html) or
	[`eprintln!`](https://doc.rust-lang.org/std/macro.eprintln.html): for that, you would need
	to use the undocumented feature [`set_panic`](https://github.com/rust-lang/rust/issues/31343).

		//silences error-reporting, epr!(), eprn!(), epr and eprn
		glsp::set_pr_writer(Box::new(std::io::sink()));
	*/

	pub fn set_epr_writer(epr_writer: Box<dyn Write>) {
		with_engine(|engine| {
			*engine.epr_writer.borrow_mut() = epr_writer;
		})
	}
	
	//---------------------------------------------------------------------------------------------
	// spans and stack-tracing
	//---------------------------------------------------------------------------------------------

	pub(crate) fn push_frame(frame: Frame) {
		with_engine(|engine| {
			engine.vm.push_frame(frame);
		})
	}

	pub(crate) fn pop_frame() {
		with_engine(|engine| {
			engine.vm.pop_frame();
		})
	}

	pub(crate) fn enter_expander(arr: &Arr, env: Rc<Env>) -> Option<(Option<Sym>, Span, Rc<Env>)> {
		with_engine(|engine| {
			let name = if arr.len() >= 1 && arr.get::<Val>(0).unwrap().is_sym() {
				Some(arr.get::<Sym>(0).unwrap())
			} else {
				None
			};
			let callsite = arr.span();

			let prev_in_expander = engine.in_expander.borrow().clone();
			*engine.in_expander.borrow_mut() = Some((name, callsite, env));

			prev_in_expander
		})
	}

	pub(crate) fn env() -> Option<Rc<Env>> {
		with_engine(|engine| {
			match *engine.in_expander.borrow() {
				Some((_, _, ref env)) => Some(Rc::clone(env)),
				None => None
			}
		})
	}

	pub(crate) fn leave_expander(prev: Option<(Option<Sym>, Span, Rc<Env>)>) {
		with_engine(|engine| {
			*engine.in_expander.borrow_mut() = prev;
		})
	}

	pub(crate) fn span(storage: SpanStorage) -> Span {
		with_engine(|engine| {
			if storage == SpanStorage::Generated {
				Span(0)
			} else {
				let mut spans_map = engine.spans_map.borrow_mut();
				match spans_map.entry(storage) {
					Occupied(entry) => *entry.get(),
					Vacant(entry) => {
						let mut spans = engine.spans.borrow_mut();
						spans.push(storage);
						
						let span = Span((spans.len() - 1) as u32);
						entry.insert(span);
						span
					}
				}
			}
		})
	}

	pub(crate) fn generated_span() -> Span {
		Span(0)
	}	

	pub(crate) fn span_storage(span: Span) -> SpanStorage {
		with_engine(|engine| {
			engine.spans.borrow()[span.0 as usize]
		})
	}

	//simplifies a Span into a brief file location, e.g. "scripts/main.glsp:10". only used by (try)
	//and (file-location). if this span was Loaded from a file, or transitively expanded from a 
	//macro callsite which was itself Loaded from a file, writes the file location to `f` and
	//returns Ok(true). otherwise, returns Ok(false).
	pub(crate) fn span_file_location<F>(f: &mut F, mut span: Span) -> Result<bool, fmt::Error>
	where
		F: fmt::Write
	{
		loop {
			match glsp::span_storage(span) {
				SpanStorage::Expanded(_, macro_invocation_span, _) => {
					span = macro_invocation_span;
				}
				SpanStorage::Loaded(file_id, line) => {
					write!(f, "{}:{}", &glsp::filename_str(file_id), line)?;
					return Ok(true)
				}
				SpanStorage::Generated => {
					return Ok(false)
				}
			}
		}
	}

	//span_context outputs a description of a single Span to surround a gfn call, rfn call, or
	//bail_at!() in a stack trace. e.g.:
	//
	//	(some-macro) at scripts/main.glsp:10
	//		expanded to (other-macro) at scripts/main.glsp:20
	//		expanded to (some-call) at scripts/main.glsp:30
	//
	pub(crate) fn span_context<F, C>(
		f: &mut F,
		span: Span,
		callback: &C
	) -> fmt::Result
	where
		F: fmt::Write,
		C: ?Sized + Fn(&mut F) -> fmt::Result
	{
		match glsp::span_storage(span) {
			SpanStorage::Generated => {
				callback(f)?;
			}
			SpanStorage::Loaded(file_id, line_number) => {
				callback(f)?;
				write!(f, " at {}:{}", &glsp::filename_str(file_id), line_number)?;
			}
			SpanStorage::Expanded(macro_name, expander_callsite, mut constructor_callsite) => {
				let print_macro_name: &dyn Fn(&mut F) -> fmt::Result = &|f| {
					match macro_name {
						Some(macro_name) => write!(f, "({})", macro_name),
						None => write!(f, "an anonymous macro")
					}
				};

				span_context(f, expander_callsite, print_macro_name)?;

				write!(f, "\n    expanded to ")?;
				callback(f)?;

				//within a macro, it's possible that arr constructors may be themselves expanded
				//from a macro. we could potentially recurse in that case, but for now we just
				//report the macro invocation's own constructor site as the construction site.
				while let SpanStorage::Expanded(_, _, meta) = 
				          glsp::span_storage(constructor_callsite) {
					constructor_callsite = meta;
				}

				match glsp::span_storage(constructor_callsite) {
					SpanStorage::Generated => (),
					SpanStorage::Loaded(file_id, line_number) => {
						write!(f, " at {}:{}", &glsp::filename_str(file_id), line_number)?;
					}
					SpanStorage::Expanded(..) => unreachable!()
				}
			}
		}

		Ok(())
	}

	/** Equivalent to [`(file-location)`](https://gamelisp.rs/std/file-location). */

	//error messages can be stringified in either a short form, or a long form (a stack trace).
	//the short form is a brief file location, followed by the stringification of the arguments
	//passed to bail!(), ensure!() or (err). this method generates that brief file location.
	//the user can also generate this directly using the (file-location) builtin fn.
	pub fn file_location() -> Option<String> {
		with_engine(|engine| {
			let mut builder = String::new();
			if engine.vm.file_location(&mut builder).unwrap() {
				Some(builder)
			} else {
				None
			}
		})
	}

	/** Equivalent to [`(stack-trace)`](https://gamelisp.rs/std/stack-trace). */

	//this method generates the long form: a full stack trace. we don't emit any leading or
	//trailing linebreaks.
	pub fn stack_trace() -> String {
		with_engine(|engine| {
			let mut builder = String::new();
			engine.vm.stack_trace(&mut builder).unwrap();
			builder
		})
	}

	//---------------------------------------------------------------------------------------------
	// try
	//---------------------------------------------------------------------------------------------

	/**
	Calls a function with either verbose or brief error-reporting.

	[`try`](https://gamelisp.rs/std/try) and [`try-verbose`](https://gamelisp.rs/std/try-verbose)
	work by invoking `glsp::try_call`.

	The default error-reporting style is verbose.
	*/
	pub fn try_call<R, A>(is_verbose: bool, receiver: &R, args: &A) -> GResult<Val>
	where
		R: CallableOps,
		A: ToCallArgs + ?Sized
	{
		with_engine(|engine| {
			let prev = engine.errors_verbose.get();
			engine.errors_verbose.set(is_verbose);
			let _guard = Guard::new(|| engine.errors_verbose.set(prev));

			glsp::call(receiver, args)
		})
	}

	pub(crate) fn errors_verbose() -> bool {
		with_engine(|engine| {
			engine.errors_verbose.get()
		})
	}

	//---------------------------------------------------------------------------------------------
	// allocation
	//---------------------------------------------------------------------------------------------

	pub(crate) fn alloc<T: Allocate>(t: T) -> Root<T> {
		with_engine(|engine| {
			engine.heap.alloc(t)
		})
	}

	#[doc(hidden)]
	pub fn alloc_gc<T: Allocate>(t: T) -> Gc<T> {
		with_engine(|engine| {
			engine.heap.alloc_gc(t)
		})
	}

	//returns the Span which should be assigned to a newly-allocated arr, allocated at `callsite`
	pub(crate) fn new_arr_span(callsite: Option<Span>) -> Span {
		with_engine(|engine| {
			if let Some(callsite) = callsite {
				if let Some((expander_name, expander_callsite, _)) = *engine.in_expander.borrow() {
					glsp::span(SpanStorage::Expanded(expander_name, expander_callsite, callsite))
				} else {
					Span::default()
				}
			} else {
				if engine.vm.in_expander() {
					//when `callsite` is None, that means the arr is being allocated by a call
					//to glsp::arr(), glsp::call(), etc., rather than an OpArr instr or something 
					//else which can pass in an explicit "current span". under those circumstances, 
					//the innermost available Span will be the callsite of the innermost active 
					//gfn/rfn call, if any, within the innermost expander - or the callsite of
					//the expander itself, otherwise. 

					//possible future extension (todo) would be to add a type of Span which refers
					//to a line of rust code, rather than a line of glsp code. couldn't use this
					//for glsp::call, but we could use it for macros like arr![] and backquote!().

					engine.vm.expander_cur_span()
				} else {
					Span::default()
				}
			}
		})
	}

	///Constructs an empty [array](struct.Arr.html).
	pub fn arr() -> Root<Arr> {
		let arr = with_heap(|heap| heap.recycler.arr());
		arr.set_span(glsp::new_arr_span(None));
		arr
	}

	///Constructs an empty [array](struct.Arr.html) with space for at least `capacity` elements.
	pub fn arr_with_capacity(capacity: usize) -> Root<Arr> {
		let arr = with_heap(|heap| heap.recycler.arr_with_capacity(capacity));
		arr.set_span(glsp::new_arr_span(None));
		arr
	}

	/**
	Constructs an [array](struct.Arr.html) which contains `reps` repetitions of `elem`.

	Returns an `Err` if [type conversion](trait.ToVal.html) fails.
	*/
	pub fn arr_from_elem<T: ToVal>(elem: T, reps: usize) -> GResult<Root<Arr>> {
		let arr = with_heap(|heap| heap.recycler.arr_from_elem(elem, reps))?;
		arr.set_span(glsp::new_arr_span(None));
		Ok(arr)
	}

	/**
	Constructs an [array](struct.Arr.html) from the contents of a Rust iterator.

	Returns an `Err` if [type conversion](trait.ToVal.html) fails for any element.
	*/
	pub fn arr_from_iter<T>(iter: T) -> GResult<Root<Arr>> 
	where
		T: IntoIterator,
		T::Item: ToVal
	{
		let arr = with_heap(|heap| heap.recycler.arr_from_iter(iter))?;
		arr.set_span(glsp::new_arr_span(None));
		Ok(arr)
	}

	///Constructs an empty [string](struct.Str.html).
	pub fn str() -> Root<Str> {
		glsp::alloc(Str::new())
	}

	///Constructs an empty [string](struct.Str.html) with the same contents as a Rust string slice.
	pub fn str_from_rust_str(src: &str) -> Root<Str> {
		glsp::alloc(Str::from_rust_str(src))
	}

	/**
	Constructs a [string](struct.Str.html) from the characters in a Rust iterator.

	Returns an `Err` if [type conversion](trait.IntoElement.html) fails for any element.
	*/
	pub fn str_from_iter<T>(iter: T) -> GResult<Root<Str>> 
	where
		T: IntoIterator,
		T::Item: IntoElement<char>
	{
		Ok(glsp::alloc(Str::from_iter(iter)?))
	}

	///Constructs an empty [string](struct.Str.html) with space for at least `capacity` characters.
	pub fn str_with_capacity(capacity: usize) -> Root<Str> {
		glsp::alloc(Str::with_capacity(capacity))
	}

	///Constructs an empty [table](struct.Tab.html).
	pub fn tab() -> Root<Tab> {
		glsp::alloc(Tab::new())
	}

	/**
	Constructs a [table](struct.Tab.html) from the key/value pairs in a Rust iterator.

	Duplicate keys are permitted.

	Returns an `Err` if [type conversion](trait.ToVal.html) fails for any key or value.
	*/
	pub fn tab_from_iter<T, K, V>(iter: T) -> GResult<Root<Tab>> 
	where
		T: IntoIterator<Item = (K, V)>,
		K: ToVal,
		V: ToVal
	{
		Ok(glsp::alloc(Tab::from_iter(iter)?))
	}

	///Constructs an empty [table](struct.Tab.html) with space for at least `capacity` elements.
	pub fn tab_with_capacity(capacity: usize) -> Root<Tab> {
		glsp::alloc(Tab::with_capacity(capacity))
	}

	#[doc(hidden)]
	pub fn class(raw_class: &Tab) -> GResult<Root<Class>> {
		Ok(glsp::alloc(Class::new(raw_class)?))
	}

	pub(crate) fn call_class(class: &Root<Class>, arg_count: usize) -> GResult<Root<Obj>> {
		with_engine(|engine| {

			//the simplest way to implement this (but not the most performant - todo fix) is 
			//to collect the arguments into a SmallVec. we leave them on the stack until the 
			//constructor returns, so that they stay rooted.
			let stacks = engine.vm.stacks.borrow();
			let base_reg = stacks.regs.len() - arg_count;

			let _guard = Guard::new(|| {
				let mut stacks = engine.vm.stacks.borrow_mut();
				stacks.regs.truncate(base_reg);
			});

			let mut args = SmallVec::<[Slot; 16]>::new();
			for i in base_reg .. stacks.regs.len() {
				//this particular incantation is much faster than iter().cloned() for some reason
				args.push(stacks.regs[i].clone());
			}

			drop(stacks);

			Obj::new(class, &args[..])
		})
	}

	//---------------------------------------------------------------------------------------------
	// iterators
	//---------------------------------------------------------------------------------------------

	pub(crate) fn giter(state: GIterState) -> Root<GIter> {
		with_heap(|heap| heap.recycler.giter(state))
	}

	/** Equivalent to [`(rn start end step-by)`](https://gamelisp.rs/std/rn). */

	pub fn rn(start: Num, end: Option<Num>, step_by: Num) -> GResult<Root<GIter>> {
		ensure!(step_by != Num::Int(0), "a range can't be stepped by 0");
		match (start, end, step_by) {
			(Num::Int(start), Some(Num::Int(end)), Num::Int(step_by)) => {
				Ok(glsp::giter(GIterState::RnExclusive(start, end, step_by)))
			}
			(Num::Int(start), None, Num::Int(step_by)) => {
				Ok(glsp::giter(GIterState::RnOpen(start, step_by)))
			}
			(start, Some(end), step_by) => {
				Ok(glsp::giter(GIterState::FRnExclusive(
					start.into_f32(),
					end.into_f32(),
					step_by.into_f32())))
			}
			(start, None, step_by) => {
				Ok(glsp::giter(GIterState::FRnOpen(start.into_f32(), step_by.into_f32())))
			}
		}
	}

	/** Equivalent to [`(rni start end step-by)`](https://gamelisp.rs/std/rni). */

	pub fn rni(start: Num, end: Option<Num>, step_by: Num) -> GResult<Root<GIter>> {
		ensure!(step_by != Num::Int(0), "a range can't be stepped by 0");
		match (start, end, step_by) {
			(Num::Int(start), Some(Num::Int(end)), Num::Int(step_by)) => {
				Ok(glsp::giter(GIterState::RnInclusive(start, end, step_by)))
			}
			(Num::Int(start), None, Num::Int(step_by)) => {
				Ok(glsp::giter(GIterState::RnOpen(start, step_by)))
			}
			(start, Some(end), step_by) => {
				Ok(glsp::giter(GIterState::FRnInclusive(
					start.into_f32(),
					end.into_f32(),
					step_by.into_f32())))
			}
			(start, None, step_by) => {
				Ok(glsp::giter(GIterState::FRnOpen(start.into_f32(), step_by.into_f32())))
			}
		}
	}

	/** Equivalent to [`(once ..args)`](https://gamelisp.rs/std/once). */

	pub fn once(args: &[Val]) -> Root<GIter> {
		match args.len() {
			0 => glsp::giter(GIterState::Empty),
			1 => glsp::giter(GIterState::Once1(Slot::from_val(&args[0]))),
			_ => {
				let arr = glsp::arr_from_iter(args.iter()).unwrap();
				glsp::giter(GIterState::OnceN(arr.to_gc()))
			}
		}
	}

	/** Equivalent to [`(once-with f)`](https://gamelisp.rs/std/once-with). */

	pub fn once_with(callable: Callable) -> Root<GIter> {
		glsp::giter(GIterState::OnceWith(GcCallable::from_callable(&callable)))
	}

	/** Equivalent to [`(repeat ..args)`](https://gamelisp.rs/std/repeat). */

	pub fn repeat(args: &[Val]) -> GResult<Root<GIter>> {
		match args.len() {
			0 => bail!("the repeat iterator requires at least one argument"),
			1 => Ok(glsp::giter(GIterState::Repeat1(Slot::from_val(&args[0])))),
			_ => {
				let arr = glsp::arr_from_iter(args.iter()).unwrap();
				Ok(glsp::giter(GIterState::RepeatN(arr.to_gc(), 0, (arr.len() - 1) as u32)))
			}
		}
	}

	/** Equivalent to [`(repeat-with f)`](https://gamelisp.rs/std/repeat-with). */

	pub fn repeat_with(callable: Callable) -> Root<GIter> {
		glsp::giter(GIterState::RepeatWith(GcCallable::from_callable(&callable)))
	}

	/** Equivalent to [`(chunks len src-arr)`](https://gamelisp.rs/std/chunks). */

	pub fn chunks(chunk_len: usize, src_arr: &Root<Arr>) -> GResult<Root<GIter>> {
		ensure!(chunk_len > 0, "cannot split an array into chunks of length 0");
		Ok(glsp::giter(GIterState::Chunks(chunk_len as u32, src_arr.shallow_clone().to_gc())))
	}

	/** Equivalent to [`(chunks-exact len src-arr)`](https://gamelisp.rs/std/chunks-exact). */

	pub fn chunks_exact(chunk_len: usize, src_arr: &Root<Arr>) -> GResult<Root<GIter>> {
		ensure!(chunk_len > 0, "cannot split an array into chunks of length 0");
		
		let len = src_arr.len() - (src_arr.len() % chunk_len);
		let arr = glsp::arr_from_iter(src_arr.iter().take(len)).unwrap();

		Ok(glsp::giter(GIterState::Chunks(chunk_len as u32, arr.to_gc())))
	}

	/** Equivalent to [`(rchunks len src-arr)`](https://gamelisp.rs/std/rchunks). */

	pub fn rchunks(chunk_len: usize, src_arr: &Root<Arr>) -> GResult<Root<GIter>> {
		ensure!(chunk_len > 0, "cannot split an array into chunks of length 0");
		Ok(glsp::giter(GIterState::RChunks(chunk_len as u32, src_arr.shallow_clone().to_gc())))
	}

	/** Equivalent to [`(rchunks-exact len src-arr)`](https://gamelisp.rs/std/rchunks-exact). */

	pub fn rchunks_exact(chunk_len: usize, src_arr: &Root<Arr>) -> GResult<Root<GIter>> {
		ensure!(chunk_len > 0, "cannot split an array into chunks of length 0");
		
		let to_skip = src_arr.len() % chunk_len;
		let arr = glsp::arr_from_iter(src_arr.iter().skip(to_skip)).unwrap();

		Ok(glsp::giter(GIterState::RChunks(chunk_len as u32, arr.to_gc())))
	}

	/** Equivalent to [`(windows len src-arr)`](https://gamelisp.rs/std/windows). */

	pub fn windows(window_len: usize, src_arr: &Root<Arr>) -> GResult<Root<GIter>> {
		ensure!(window_len > 0, "cannot split an array into windows of length 0");
		Ok(glsp::giter(GIterState::Windows(window_len as u32, src_arr.shallow_clone().to_gc())))
	}

	/** Equivalent to [`(lines st)`](https://gamelisp.rs/std/lines). */

	pub fn lines(st: &Root<Str>) -> Root<GIter> {
		glsp::giter(GIterState::Lines(st.shallow_clone().to_gc()))
	}

	/** Equivalent to [`(split st split-at)`](https://gamelisp.rs/std/split). */

	pub fn split(src: &Root<Str>, split_at: &Root<Str>) -> Root<GIter> {
		glsp::giter(GIterState::Split(src.shallow_clone().to_gc(),
		                              split_at.shallow_clone().to_gc()))
	}

	/** Equivalent to [`(rev base)`](https://gamelisp.rs/std/rev). */

	pub fn rev(base: &Root<GIter>) -> GResult<Root<GIter>> {
		ensure!(base.is_double_ended(), "{} iterators are not double-ended", base.state_name());
		Ok(glsp::giter(GIterState::Rev(base.to_gc())))
	}

	/** Equivalent to [`(enumerate base)`](https://gamelisp.rs/std/enumerate). */

	pub fn enumerate(base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Enumerate(base.to_gc(), 0))
	}

	/** Equivalent to [`(cloned base)`](https://gamelisp.rs/std/cloned). */

	pub fn cloned(base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Cloned(base.to_gc()))
	}

	/** Equivalent to [`(deep-cloned base)`](https://gamelisp.rs/std/deep-cloned). */

	pub fn deep_cloned(base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::DeepCloned(base.to_gc()))
	}

	/** Equivalent to [`(step-by n base)`](https://gamelisp.rs/std/step-by). */

	pub fn step_by(step_by: usize, base: &Root<GIter>) -> GResult<Root<GIter>> {
		ensure!(step_by > 0, "cannot step an iterator by 0");
		Ok(glsp::giter(GIterState::StepBy(step_by as u32, base.to_gc())))
	}

	/** Equivalent to [`(map f base)`](https://gamelisp.rs/std/map). */

	pub fn map(callable: &Callable, base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Map(GcCallable::from_callable(callable), base.to_gc()))
	}

	/** Equivalent to [`(filter f base)`](https://gamelisp.rs/std/filter). */

	pub fn filter(callable: &Callable, base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Filter(GcCallable::from_callable(callable), base.to_gc()))
	}

	/** Equivalent to [`(zip ..args)`](https://gamelisp.rs/std/zip). */

	pub fn zip(iterables: &[Iterable]) -> Root<GIter> {
		let arr = glsp::arr_from_iter(iterables.iter().map(|iterable| iterable.giter())).unwrap();
		glsp::giter(GIterState::Zip(arr.to_gc()))
	}

	/** Equivalent to [`(chain ..args)`](https://gamelisp.rs/std/chain). */

	pub fn chain(iterables: &[Iterable]) -> Root<GIter> {
		let arr = glsp::arr_from_iter(iterables.iter().map(|iterable| iterable.giter())).unwrap();
		glsp::giter(GIterState::Chain(arr.to_gc()))
	}

	/** Equivalent to [`(flatten base)`](https://gamelisp.rs/std/flatten). */

	pub fn flatten(base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Flatten(base.to_gc(), None))
	}

	/** Equivalent to [`(cycle base)`](https://gamelisp.rs/std/cycle). */

	pub fn cycle(base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Cycle(Some(base.to_gc()), glsp::arr().to_gc(), 0))
	}

	/** Equivalent to [`(take n base)`](https://gamelisp.rs/std/take). */

	pub fn take(n: usize, base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Take(n as u32, base.to_gc()))
	}

	/** Equivalent to [`(take-while f base)`](https://gamelisp.rs/std/take-while). */

	pub fn take_while(callable: &Callable, base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::TakeWhile(GcCallable::from_callable(callable), base.to_gc()))
	}

	/** Equivalent to [`(skip n base)`](https://gamelisp.rs/std/skip). */

	pub fn skip(n: usize, base: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::Skip(n as u32, base.to_gc()))
	}

	/** Equivalent to [`(skip-while f base)`](https://gamelisp.rs/std/skip-while). */

	pub fn skip_while(callable: &Callable, base: &Root<GIter>) -> Root<GIter> {
		let gc_callable = GcCallable::from_callable(callable);
		glsp::giter(GIterState::SkipWhile(Some(gc_callable), base.to_gc()))
	}

	//---------------------------------------------------------------------------------------------
	// garbage collection
	//---------------------------------------------------------------------------------------------

	/** Equivalent to [`(gc)`](https://gamelisp.rs/std/gc). */

	pub fn gc() {
		with_engine(|engine| {
			engine.vm.traverse_stacks();
			engine.heap.step()
		})
	}

	/** Equivalent to [`(gc-value 'ratio)`](https://gamelisp.rs/std/gc-value). */

	pub fn gc_ratio() -> f32 {
		with_engine(|engine| {
			engine.heap.ratio()
		})
	}

	/** Equivalent to [`(= (gc-value 'ratio) ratio)`](https://gamelisp.rs/std/set-gc-value). */

	pub fn gc_set_ratio(ratio: f32) {
		with_engine(|engine| {
			engine.heap.set_ratio(ratio)
		})
	}

	/** Equivalent to [`(gc-value 'young-bytes)`](https://gamelisp.rs/std/gc-value). */

	pub fn gc_young_bytes() -> usize {
		with_engine(|engine| {
			engine.heap.young_memory_usage()
		})
	}

	/** Equivalent to [`(gc-value 'old-bytes)`](https://gamelisp.rs/std/gc-value). */

	pub fn gc_old_bytes() -> usize {
		with_engine(|engine| {
			engine.heap.old_memory_usage()
		})
	}

	/** Equivalent to [`(gc-value 'ghost-bytes)`](https://gamelisp.rs/std/gc-value). */

	pub fn gc_ghost_bytes() -> usize {
		with_engine(|engine| {
			engine.heap.ghost_memory_usage()
		})
	}

	//---------------------------------------------------------------------------------------------
	// evaluation and expansion
	//---------------------------------------------------------------------------------------------

	/** Equivalent to [`(eval val env-mode)`](https://gamelisp.rs/std/eval). */

	pub fn eval(val: &Val, env_mode: Option<EnvMode>) -> GResult<Val> {
		glsp::push_frame(Frame::GlspApi(GlspApiName::Eval, None));
		let _guard = Guard::new(|| glsp::pop_frame());

		eval::eval(&[val.clone()], env_mode, false)
	}

	/** Equivalent to [`(eval-multi vals env-mode)`](https://gamelisp.rs/std/eval-multi). */

	pub fn eval_multi(vals: &[Val], env_mode: Option<EnvMode>) -> GResult<Val> {
		glsp::push_frame(Frame::GlspApi(GlspApiName::EvalMulti, None));
		let _guard = Guard::new(|| glsp::pop_frame());

		eval::eval(vals, env_mode, false)
	}

	/** Equivalent to [`(load filename)`](https://gamelisp.rs/std/load). */

	pub fn load(filename: &str) -> GResult<Val> {
		let file_id = glsp::filename(filename);

		glsp::push_frame(Frame::GlspApi(GlspApiName::Load, Some(file_id)));
		let _guard = Guard::new(|| glsp::pop_frame());

		#[cfg(feature = "compiler")] {
			if is_playing_back() {
				return glsp::load_playback(filename)
			} else {
				glsp::record_action(Action::StartLoad(file_id));
			}
		}

		#[cfg(feature = "compiler")]
		let _guard = Guard::new(|| glsp::record_action(Action::EndLoad));

		let text = match fs::read_to_string(&filename) {
			Ok(text) => text,
			Err(err) => return Err(error!("unable to load file '{}'", filename).with_source(err))
		};

		let vals = glsp::parse_all(&text, Some(filename))?;

		eval::eval(&vals, None, true)
	}

	/** Equivalent to [`(require filename)`](https://gamelisp.rs/std/require). */

	pub fn require(filename: &str) -> GResult<Val> {
		let file_id = glsp::filename(filename);
		glsp::push_frame(Frame::GlspApi(GlspApiName::Require, Some(file_id)));
		let _guard = Guard::new(|| glsp::pop_frame());

		let path = match PathBuf::from(filename).canonicalize() {
			Ok(path) => path,
			Err(err) => {
				let msg = error!("invalid filename '{}' passed to glsp::require", filename);
				return Err(msg.with_source(err))
			}
		};
		
		let already_seen = with_engine(|engine| {
			let mut required = engine.required.borrow_mut();
			if required.contains(&path) {
				return true
			} else {
				required.insert(path);
				return false
			}
		});

		if already_seen {
			Ok(Val::Nil)
		} else {
			glsp::load(filename)
		}
	}

	/**
	Loads a file and serializes its compiled bytecode to a `Vec<u8>`.
	
	See the [Compilation](https://gamelisp.rs/reference/compilation.html) chapter of the
	manual for more details.
	*/

	#[cfg(feature = "compiler")]
	pub fn load_and_compile(filename: &str) -> GResult<(Val, Vec<u8>)> {
		let file_id = glsp::filename(filename);
		glsp::push_frame(Frame::GlspApi(GlspApiName::LoadAndCompile, Some(file_id)));
		let _guard = Guard::new(|| glsp::pop_frame());

		let text = match fs::read_to_string(&filename) {
			Ok(text) => text,
			Err(err) => return Err(error!("unable to load file '{}'", filename).with_source(err))
		};

		glsp::load_and_compile_str(&text, filename)
	}

	#[doc(hidden)]
	#[cfg(feature = "compiler")]
	pub fn load_and_compile_str(
		content: &str,
		filename: &str
	) -> GResult<(Val, Vec<u8>)> {

		let file_id = glsp::filename(filename);
		glsp::push_frame(Frame::GlspApi(GlspApiName::LoadAndCompileStr, Some(file_id)));
		let _guard = Guard::new(|| glsp::pop_frame());

		let vals = glsp::parse_all(content, Some(filename))?;
		glsp::load_and_compile_vals(&vals[..], filename)
	}

	#[doc(hidden)]
	#[cfg(feature = "compiler")]
	pub fn load_and_compile_vals(
		vals: &[Val],
		filename: &str
	) -> GResult<(Val, Vec<u8>)> {

		let file_id = glsp::filename(filename);
		glsp::push_frame(Frame::GlspApi(GlspApiName::LoadAndCompileVals, Some(file_id)));
		let _guard = Guard::new(|| glsp::pop_frame());

		with_engine(|engine| {
			ensure!(engine.playing_back.borrow().is_none(), 
			        "cannot load_and_compile and load_compiled simultaneously");

			let mut recording = engine.recording.borrow_mut();
			ensure!(recording.is_none(), "multiple simultaneous calls to load_and_compile");
			*recording = Some(Recording::new());

			Ok(())
		})?;
		let recording_guard = Guard::new(|| {
			with_engine(|engine| {
				*engine.recording.borrow_mut() = None;
			})
		});

		glsp::seed_gensym();

		glsp::record_action(Action::StartLoad(file_id));
		let end_load_guard = Guard::new(|| glsp::record_action(Action::EndLoad));

		let result = eval::eval(vals, None, true)?;

		forget(recording_guard);
		drop(end_load_guard);

		let bytes = with_engine(|engine| {
			engine.recording.borrow_mut().take().unwrap().into_bytes()
		});

		Ok((result, bytes))
	}

	#[cfg(feature = "compiler")]
	pub(crate) fn record_action(action: Action) {
		with_engine(|engine| {
			let mut recording = engine.recording.borrow_mut();
			if let Some(ref mut recording) = *recording {
				recording.add_action(action);
			}
		});
	}

	#[cfg(feature = "compiler")]
	pub(crate) fn is_playing_back() -> bool {
		with_engine(|engine| {
			engine.playing_back.borrow().is_some()
		})
	}

	#[cfg(feature = "compiler")]
	pub(crate) fn pop_action() -> GResult<Action> {
		with_engine(|engine| {
			engine.playing_back.borrow_mut().as_mut().unwrap().pop()
		})
	}

	/**
	Loads a file which was previously serialized using 
	[`glsp::load_and_compile`](fn.load_and_compile.html).
	
	See the [Compilation](https://gamelisp.rs/reference/compilation.html) chapter of the
	manual for more details.
	*/

	#[cfg(feature = "compiler")]
	pub fn load_compiled(bytes: &[u8]) -> GResult<Val> {
		glsp::push_frame(Frame::GlspApi(GlspApiName::LoadCompiled, None));
		let _guard = Guard::new(|| glsp::pop_frame());

		let recording = Recording::from_bytes(bytes)?;

		let root_filename = match recording.peek()? {
			&Action::StartLoad(filename) => filename,
			_ => bail!("invalid Recording: first Action must be StartLoad")
		};

		with_engine(|engine| {
			ensure!(engine.recording.borrow().is_none(), 
			        "cannot load_and_compile and load_compiled simultaneously");

			let mut playing_back = engine.playing_back.borrow_mut();
			ensure!(playing_back.is_none(), "multiple simultaneous calls to load_compiled");
			*playing_back = Some(recording);

			Ok(())
		})?;
		let playing_back_guard = Guard::new(|| {
			with_engine(|engine| {
				*engine.playing_back.borrow_mut() = None;
			});
		});

		let result = glsp::load_playback(&glsp::filename_str(root_filename))?;

		forget(playing_back_guard);

		let recording = with_engine(|engine| {
			engine.playing_back.borrow_mut().take().unwrap()
		});

		ensure!(recording.is_empty(), "invalid Recording: some Actions are unused");

		Ok(result)
	}

	//glsp::load delegates to this function when glsp::is_playing_back() is true.
	#[cfg(feature = "compiler")]
	pub(crate) fn load_playback(expected_filename: &str) -> GResult<Val> {
		assert!(glsp::is_playing_back());

		match glsp::pop_action()? {
			Action::StartLoad(recorded_filename) 
				if expected_filename == &*glsp::filename_str(recorded_filename) => (),
			_ => bail!("invalid Recording: unexpected call to (load {:?})", expected_filename)
		}

		let mut result = Val::Nil;
		let mut toplevel_let: Option<Root<Stay>> = None;
		loop {
			match glsp::pop_action()? {
				Action::Execute(bytecode) => {
					with_vm(|vm| {
						result = vm.exec_bytecode(&bytecode)?;
						Ok(())
					})?;

					if let Some(stay) = toplevel_let.take() {
						stay.set(Slot::from_val(&result));
						result = Val::Nil;
					}
				}
				Action::ToplevelLet(stay) => {
					ensure!(toplevel_let.is_none(), "invalid Recording: unexpected ToplevelLet");
					toplevel_let = Some(stay);
				}
				Action::StartLoad(filename) => {
					bail!("invalid Recording: unexpected StartLoad({})", 
					      glsp::filename_str(filename))
				}
				Action::EndLoad => {
					ensure!(toplevel_let.is_none(), "invalid Recording: unexpected ToplevelLet");
					return Ok(result)
				}
			}
		}
	}

	/**
	Invokes a callable value: an [`RFn`](struct.RFn.html), [`GFn`](struct.GFn.html) or 
	[`Class`](struct.Class.html).

	Note that both the `receiver` and the `args` are passed by reference. The `args` should be
	a reference to `()`, a tuple, a slice, or a fixed-size array.

		let rect: Root<Obj> = glsp::call(&rect_class, &[10, 10, 50, 50])?;
	*/

	pub fn call<C, A, R>(receiver: &C, args: &A) -> GResult<R>
	where
		C: CallableOps,
		A: ToCallArgs + ?Sized,
		R: FromVal
	{
		glsp::push_frame(Frame::GlspCall(receiver.name()));
		let _guard = Guard::new(|| glsp::pop_frame());

		with_engine(|engine| {
			let mut stacks = engine.vm.stacks.borrow_mut();
			let starting_len = stacks.regs.len();

			args.to_call_args(&mut stacks.regs)?;

			let arg_count = stacks.regs.len() - starting_len;
			drop(stacks);

			R::from_val(&receiver.receive_call(arg_count)?)
		})
	}

	pub(crate) fn call_gfn(gfn: &Root<GFn>, arg_count: usize) -> GResult<Val> {
		with_engine(|engine| {
			Ok(engine.vm.exec_gfn(gfn, arg_count)?)
		})
	}

	/** Equivalent to [`(coro-run co arg)`](https://gamelisp.rs/std/coro-run). */

	pub fn coro_run(coro: &Root<Coro>, resume_arg: Option<Val>) -> GResult<Val> {
		glsp::push_frame(Frame::GlspCoroRun(coro.to_gc()));
		let _guard = Guard::new(|| glsp::pop_frame());

		with_engine(|engine| {
			Ok(engine.vm.coro_run(coro, resume_arg)?)
		})
	}

	/** Equivalent to [`(coro-finish! co)`](https://gamelisp.rs/std/coro-finish-mut). */

	pub fn coro_finish(coro: &Root<Coro>) -> GResult<()> {
		glsp::push_frame(Frame::GlspApi(GlspApiName::CoroFinish, None));
		let _guard = Guard::new(|| glsp::pop_frame());

		with_engine(|engine| {
			Ok(engine.vm.coro_finish(coro)?)
		})
	}

	/** Equivalent to [`(expand val env-mode)`](https://gamelisp.rs/std/expand) */

	pub fn expand(val: &Val, env_mode: Option<EnvMode>) -> GResult<Val> {
		glsp::push_frame(Frame::GlspApi(GlspApiName::Expand, None));
		let _guard = Guard::new(|| glsp::pop_frame());

		let mut expanded = eval::expand(&[val.clone()], env_mode, true)?;
		assert!(expanded.len() == 1);
		Ok(expanded.pop().unwrap())
	}

	/** Equivalent to [`(expand-multi vals env-mode)`](https://gamelisp.rs/std/expand-multi) */

	pub fn expand_multi(vals: &[Val], env_mode: Option<EnvMode>) -> GResult<Vec<Val>> {
		glsp::push_frame(Frame::GlspApi(GlspApiName::ExpandMulti, None));
		let _guard = Guard::new(|| glsp::pop_frame());

		eval::expand(vals, env_mode, false)
	}

	/** Equivalent to [`(expand-1 env-mode val expander)`](https://gamelisp.rs/std/expand-1) */

	pub fn expand_1(
		form: &Val, 
		expander: Option<Expander>,
		env_mode: Option<EnvMode>
	) -> GResult<Expansion> {
		glsp::push_frame(Frame::GlspApi(GlspApiName::Expand1, None));
		let _guard = Guard::new(|| glsp::pop_frame());

		eval::expand_1(form, expander, env_mode)
	}
}


//-------------------------------------------------------------------------------------------------
// stock_syms
//-------------------------------------------------------------------------------------------------

macro_rules! define_stock_syms(
	($($kind:ident : $(($sym_str:literal, $sym_name:ident)),+),+) => (
		#[derive(Copy, Clone, Debug)]
		#[allow(non_camel_case_types)]
		pub(crate) enum StockSym {
			$($($sym_name),+),+,

			STOCK_SYM_COUNT
		}

		const STOCK_SYMS: [(&str, SymKind); StockSym::STOCK_SYM_COUNT as usize] = [
			$($(($sym_str, SymKind::$kind)),+),+
		];


		const STOCK_SYMS_BY_KIND: [&[Sym]; 3] = [
			$(&[$(Sym(StockSym::$sym_name as u32, PhantomData)),+]),+
		];

		#[doc(hidden)]
		pub mod stock_syms {
			use std::marker::{PhantomData};
			use super::{StockSym, Sym};

			$($(
				pub const $sym_name: Sym = Sym(StockSym::$sym_name as u32, PhantomData);
			)+)+
		}
	);
);

define_stock_syms!(

	StockSpecial:
		
		("do", DO_SYM),
		("quote", QUOTE_SYM),
		("if", IF_SYM),
		("let", LET_SYM),
		("fn", FN_SYM),
		("return", RETURN_SYM),
		("yield", YIELD_SYM),
		("defer", DEFER_SYM),
		("defer-yield", DEFER_YIELD_SYM),
		("block", BLOCK_SYM),
		("finish-block", FINISH_BLOCK_SYM),
		("restart-block", RESTART_BLOCK_SYM),
		("=", ASSIGNMENT_SYM),

	StockKeyword:

		("&name", FLAG_NAME_SYM),
		("&arg-limits", FLAG_ARG_LIMITS_SYM),

		("?", QUESTION_MARK_SYM),
		(":", COLON_SYM),
		("_", UNDERSCORE_SYM),

		("at", AT_SYM),
		("and", AND_SYM),
		("or", OR_SYM),

		("def", DEF_SYM),
		("with-global", WITH_GLOBAL_SYM),

		("template-str", TEMPLATE_STR_SYM),

		("newborn", NEWBORN_SYM),
		("running", RUNNING_SYM),
		("paused", PAUSED_SYM),
		("finished", FINISHED_SYM),
		("poisoned", POISONED_SYM),

		("splice", SPLICE_SYM),
		("let-macro", LET_MACRO_SYM),
		("expanded-to", EXPANDED_TO_SYM),
		("macro-no-op", MACRO_NO_OP_SYM),
		("not-a-macro", NOT_A_MACRO_SYM),
		("fresh", FRESH_SYM),
		("copied", COPIED_SYM),

		("ok", OK_SYM),
		("err", ERR_SYM),
		("brief", BRIEF_SYM),
		("verbose", VERBOSE_SYM),
		("end-of-input", END_OF_INPUT_SYM),

		("else", ELSE_SYM),
		("same?", SAMEP_SYM),
		("eq?", EQP_SYM),
		("cond", COND_SYM),
		("any-of", ANY_OF_SYM),
		("in", IN_SYM),

		("backquote", BACKQUOTE_SYM),
		("unquote", UNQUOTE_SYM),
		("splay", SPLAY_SYM),
		("met-name", MET_NAME_SYM),
		("atsign", ATSIGN_SYM),
		("atsign=", SET_ATSIGN_SYM),
		("atsign-opt", ATSIGN_OPT_SYM),
		("atsign-opt=", SET_ATSIGN_OPT_SYM),
		("access-opt=", SET_ACCESS_OPT_SYM),

		("nil", NIL_SYM),
		("char", CHAR_SYM),
		("sym", SYM_SYM),
		("rfn", RFN_SYM),
		("str", STR_SYM),
		("tab", TAB_SYM),
		("obj", OBJ_SYM),
		("class", CLASS_SYM),
		("coro", CORO_SYM),
		("rdata", RDATA_SYM),

		("infinite", INFINITE_SYM),
		("unknown", UNKNOWN_SYM),

		("field", FIELD_SYM),
		("const", CONST_SYM),
		("met", MET_SYM),
		("wrap", WRAP_SYM),
		("wildcard-wrap", WILDCARD_WRAP_SYM),
		("prop", PROP_SYM),
		("wrap-prop", WRAP_PROP_SYM),
		("wildcard-wrap-prop", WILDCARD_WRAP_PROP_SYM),
		("get", GET_SYM),
		("set", SET_SYM),
		("init", INIT_SYM),
		("init-mixin", INIT_MIXIN_SYM),
		("init-state", INIT_STATE_SYM),
		("inits", INITS_SYM),
		("fini", FINI_SYM),
		("fini-mixin", FINI_MIXIN_SYM),
		("fini-state", FINI_STATE_SYM),
		("finis", FINIS_SYM),
		("mixin", MIXIN_SYM),
		("mixin?", MIXINP_SYM),
		("name", NAME_SYM),
		("class-name", CLASS_NAME_SYM),
		("state-name", STATE_NAME_SYM),
		("self", SELF_SYM),
		("base", BASE_SYM),
		("Main", MAIN_SYM),
		("states", STATES_SYM),
		("state", STATE_SYM),
		("state*", STATEX_SYM),
		("enab!", ENAB_SYM),
		("enab?", ENABP_SYM),
		("disab!", DISAB_SYM),
		("fsm", FSM_SYM),
		("fsm-siblings", FSM_SIBLINGS_SYM),
		("parent", PARENT_SYM),
		("children", CHILDREN_SYM),
		("enabled-by-default?", ENABLED_BY_DEFAULTP_SYM),
		("bindings", BINDINGS_SYM),

		("op-clone", OP_CLONE_SYM),
		("op-deep-clone", OP_DEEP_CLONE_SYM),
		("op-eq?", OP_EQP_SYM),

		("ratio", RATIO_SYM),
		("min-ratio", MIN_RATIO_SYM),
		("default-ratio", DEFAULT_RATIO_SYM),
		("young-bytes", YOUNG_BYTES_SYM),
		("old-bytes", OLD_BYTES_SYM),
		("ghost-bytes", GHOST_BYTES_SYM),

	StockTransform:

		("+", ADD_SYM),
		("-", SUB_SYM),
		("*", MUL_SYM),
		("/", DIV_SYM),
		("%", REM_SYM),
		("abs", ABS_SYM),

		("bitand", BITAND_SYM),
		("bitor", BITOR_SYM),
		("bitxor", BITXOR_SYM),

		("sign", SIGN_SYM),
		("min", MIN_SYM),
		("max", MAX_SYM),

		("nil?", NILP_SYM),
		("num?", NUMP_SYM),
		("int?", INTP_SYM),
		("flo?", FLOP_SYM),
		("nan?", NANP_SYM),
		("inf?", INFP_SYM),
		("bool?", BOOLP_SYM),
		("sym?", SYMP_SYM),
		("deque?", DEQUEP_SYM),
		("arr?", ARRP_SYM),
		("str?", STRP_SYM),
		("tab?", TABP_SYM),
		("iter?", ITERP_SYM),
		("iterable?", ITERABLEP_SYM),
		("obj?", OBJP_SYM),
		("class?", CLASSP_SYM),
		("fn?", FNP_SYM),
		("rfn?", RFNP_SYM),
		("coro?", COROP_SYM),
		("rdata?", RDATAP_SYM),
		("callable?", CALLABLEP_SYM),
		("expander?", EXPANDERP_SYM),

		("int", INT_SYM),
		("flo", FLO_SYM),
		("bool", BOOL_SYM),

		("==", NUM_EQ_SYM),
		("<", LT_SYM),
		("<=", LTE_SYM),
		(">", GT_SYM),
		(">=", GTE_SYM),

		("not", NOT_SYM),

		("iter", ITER_SYM),
		("iter-next!", ITER_NEXT_SYM),
		("iter-next-back!", ITER_NEXT_BACK_SYM),
		("iter-finished?", ITER_FINISHEDP_SYM),

		("len", LEN_SYM),
		("has?", HASP_SYM),
		("access", ACCESS_SYM),
		("access=", SET_ACCESS_SYM),

		("arr", ARR_SYM),

		("call-met", CALL_MET_SYM),
		("call-met-opt", CALL_MET_OPT_SYM),
		("call-base-raw", CALL_BASE_RAW_SYM),

		("global", GLOBAL_SYM),
		("global=", SET_GLOBAL_SYM)

);
