use super::code::{Bytecode, Coro, GFn, Lambda, Stay};
use super::collections::{Arr, DequeOps, Str, Tab};
use super::class::{Class, Obj};
use super::engine::{ACTIVE_ENGINE_ID, glsp, GStore, RData, RFn, Span, Sym, with_heap};
use super::error::{GResult};
use super::iter::{GIter, GIterState};
use super::val::{Hashable, Val};
use super::wrap::{ToVal};
use std::{f32};
use std::borrow::{Borrow};
use std::cell::{Cell, RefCell, RefMut};
use std::cmp::{max};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::mem::{size_of};
use std::ops::{Deref};
use std::process::{abort};

//the garbage collector currently uses a hybrid incremental and generational algorithm. see
//notes/gc.md for the details.


// safe Gc<T> implementation
//----------------------------------------------------------------------------

#[cfg(not(feature = "unsafe-internals"))]
use std::rc::Rc;

#[doc(hidden)]
#[cfg(not(feature = "unsafe-internals"))]
pub struct Gc<T: Allocate> {
	rc: Rc<T>
}

#[cfg(not(feature = "unsafe-internals"))]
impl<T: Allocate> Gc<T> {
	fn new(t: T) -> Gc<T> {
		Gc {
			rc: Rc::new(t)
		}
	}

	pub(crate) fn ptr_eq(gc0: &Gc<T>, gc1: &Gc<T>) -> bool {
		Rc::ptr_eq(&gc0.rc, &gc1.rc)
	}

	fn as_usize(&self) -> usize {
		&(*self.rc) as *const T as usize
	}

	fn free(&self) {
		self.clear_gcs()
	}
}

#[cfg(not(feature = "unsafe-internals"))]
impl<T: Allocate> Clone for Gc<T> {
	fn clone(&self) -> Gc<T> {
		Gc {
			rc: Rc::clone(&self.rc)
		}
	}
}

#[cfg(not(feature = "unsafe-internals"))]
impl<T: Allocate> Deref for Gc<T> {
	type Target = T;

	fn deref(&self) -> &T {
		&*self.rc
	}
}

// unsafe Gc<T> implementation
//----------------------------------------------------------------------------

#[cfg(feature = "unsafe-internals")]
use std::ptr::NonNull;

#[doc(hidden)]
#[cfg(feature = "unsafe-internals")]
pub struct Gc<T: Allocate> {
	ptr: NonNull<T>
}

#[cfg(feature = "unsafe-internals")]
impl<T: Allocate> Gc<T> {
	fn new(t: T) -> Gc<T> {
		Gc {
			ptr: NonNull::new(Box::into_raw(Box::new(t))).unwrap()
		}
	}

	pub(crate) fn ptr_eq(gc0: &Gc<T>, gc1: &Gc<T>) -> bool {
		gc0.ptr == gc1.ptr
	}

	fn as_usize(&self) -> usize {
		self.ptr.as_ptr() as usize
	}

	fn free(&self) {
		unsafe {
			drop(Box::from_raw(self.ptr.as_ptr()))
		}
	}
}

#[cfg(feature = "unsafe-internals")]
impl<T: Allocate> Clone for Gc<T> {
	fn clone(&self) -> Gc<T> {
		Gc {
			ptr: self.ptr
		}
	}
}

#[cfg(feature = "unsafe-internals")]
impl<T: Allocate> Deref for Gc<T> {
	type Target = T;

	fn deref(&self) -> &T {
		unsafe {
			&*self.ptr.as_ptr()
		}
	}
}

// common Gc<T> methods
//----------------------------------------------------------------------------

impl<T: Allocate> Gc<T> {
	pub(crate) fn from_root(root: &Root<T>) -> Gc<T> {
		let engine_id = ACTIVE_ENGINE_ID.with(|id| id.get().unwrap());
		if engine_id != root.header().engine_id() {
			eprintln!("attempted to move a Root to another Runtime - aborting process");
			abort()
		}

		root.gc.clone()
	}

	pub(crate) fn root(&self) -> Root<T> {
		Root::new(self.clone())
	}

	pub(crate) fn into_root(self) -> Root<T> {
		Root::new(self)
	}

	fn header(&self) -> &GcHeader {
		(**self).header()
	}
}

impl<T: Allocate> PartialEq for Gc<T> {
	fn eq(&self, other: &Gc<T>) -> bool {
		self.as_usize() == other.as_usize()
	}
}

impl<T: Allocate> Eq for Gc<T> { }

impl<T: Allocate> Hash for Gc<T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.as_usize().hash(state)
	}
}

macro_rules! erased_types {
	($($type_name:ident),+) => (
		
		#[doc(hidden)]
		pub trait Erase {
			fn erase_gc(gc: Gc<Self>) -> ErasedGc where Self: Allocate;
		}

		$(impl Erase for $type_name {
			#[inline(always)]
			fn erase_gc(gc: Gc<$type_name>) -> ErasedGc {
				ErasedGc::$type_name(gc)
			}
		})+

		#[doc(hidden)]
		#[derive(Clone)]
		pub enum ErasedGc {
			$($type_name(Gc<$type_name>)),+
		}

		impl ErasedGc {
			fn header(&self) -> &GcHeader {
				match *self {
					$(ErasedGc::$type_name(ref gc) => &gc.header()),+
				}
			}
		}

		impl Debug for ErasedGc {
			fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
				match *self {
					$(
						ErasedGc::$type_name(ref gc) => {
							write!(f, "ErasedGc::{}(0x{:x})", stringify!($type_name),
							       (&**gc) as *const $type_name as usize)
						}
					)+
				}
			}
		}

		macro_rules! with_erased_gc {
			($erased:expr, $gc_ident:ident, $body:expr) => (
				match $erased {
					$(ErasedGc::$type_name(ref $gc_ident) => $body),+
				}
			);
		}
	);
}

erased_types!(
	Arr,
	Str,
	Tab,
	GIter,
	Obj,
	Class,
	GFn,
	Stay,
	Coro,
	RData,
	Bytecode,
	Lambda
);

/**
A smart pointer onto the garbage-collected heap.

*/

pub struct Root<T: Allocate> {
	pub(crate) gc: Gc<T>
}

struct RootEntry {
	gc: ErasedGc,
	root_count: usize
}

impl<T: Allocate> Root<T> {
	fn new(gc: Gc<T>) -> Root<T> {
		let header = gc.header();

		let engine_id = ACTIVE_ENGINE_ID.with(|id| id.get().unwrap());
		if engine_id != header.engine_id() {
			eprintln!("attempted to create a Root for an inactive Runtime - aborting process");
			abort()
		}
		
		with_heap(|heap| {
			if header.rooted() {
				let root_index = header.root_index();
				
				let mut roots = heap.roots.borrow_mut();
				roots[root_index].root_count += 1;
			} else {
				//add a new Gc to the `roots` vec
				let mut roots = heap.roots.borrow_mut();
				roots.push(RootEntry {
					gc: T::erase_gc(gc.clone()),
					root_count: 1
				});

				//update `root_index` for the pointed-to object
				header.set_root_index(roots.len() - 1);
			}
		});

		Root {
			gc
		}
	}

	pub(crate) fn as_gc(&self) -> &Gc<T> {
		&self.gc
	}

	pub(crate) fn to_gc(&self) -> Gc<T> {
		Gc::from_root(self)
	}

	//weirdly, there's actually no way to do this safely in current rust (todo?)
	#[doc(hidden)]
	pub fn into_gc(self) -> Gc<T> {
		Gc::from_root(&self)
	}

	pub fn ptr_eq(root0: &Root<T>, root1: &Root<T>) -> bool {
		Gc::ptr_eq(&root0.gc, &root1.gc)
	}
}

impl<T: Allocate> Borrow<T> for Root<T> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: Allocate> AsRef<T> for Root<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: Allocate + PartialEq<T>> PartialEq<Root<T>> for Root<T> {
	fn eq(&self, other: &Root<T>) -> bool {
		**self == **other
	}
}

impl<T: Allocate> Clone for Root<T> {
	fn clone(&self) -> Root<T> {
		let header = self.gc.header();
		let root_index = header.root_index();

		let engine_id = ACTIVE_ENGINE_ID.with(|id| id.get().unwrap());
		if engine_id != header.engine_id() {
			eprintln!("attempted to clone a Root for an inactive Runtime - aborting process");
			abort()
		}

		//we could eliminate this with_heap() and borrow_mut() by storing the root count inline
		//in the GcHeader, but that would enlarge GcHeader significantly. difficult decision.
		with_heap(|heap| {
			heap.roots.borrow_mut()[root_index].root_count += 1;
		});

		Root {
			gc: self.gc.clone()
		}
	}
}

impl<T: Allocate> Deref for Root<T> {
	type Target = T;
	fn deref(&self) -> &T {
		&self.gc
	}
}

impl<T: Allocate> Drop for Root<T> {
	#[inline]
	fn drop(&mut self) {
		let header = self.gc.header();
		let root_index = header.root_index();

		let engine_id = ACTIVE_ENGINE_ID.with(|id| id.get().unwrap());
		if engine_id != header.engine_id() {
			eprintln!("attempted to drop a Root for an inactive Runtime - aborting process");
			abort()
		}

		with_heap(|heap| {
			let mut roots = heap.roots.borrow_mut();

			if roots[root_index].root_count == 1 {
				//delete our root entry, swapping in the last entry so that we're not leaving a 
				//hole in the roots vec
				roots.swap_remove(root_index);
				header.unroot();

				//change the root_index for the item we just swapped, to reflect its new position
				if root_index < roots.len() {
					roots[root_index].gc.header().set_root_index(root_index);
				}
			} else {
				roots[root_index].root_count -= 1;
			}
		});
	}
}

#[doc(hidden)]
#[derive(Clone)]
pub enum Slot {
	Nil,
	Int(i32),
	Flo(f32),
	Char(char),
	Bool(bool),
	Sym(Sym),
	RFn(RFn),
	Arr(Gc<Arr>),
	Str(Gc<Str>),
	Tab(Gc<Tab>),
	GIter(Gc<GIter>),
	Obj(Gc<Obj>),
	Class(Gc<Class>),
	GFn(Gc<GFn>),
	Coro(Gc<Coro>),
	RData(Gc<RData>)
}

impl Slot {
	pub(crate) fn from_val(val: &Val) -> Slot {
		match *val {
			Val::Nil => Slot::Nil,
			Val::Int(i) => Slot::Int(i),
			Val::Char(c) => Slot::Char(c),
			Val::Flo(f) => Slot::Flo(f),
			Val::Bool(b) => Slot::Bool(b),
			Val::Sym(s) => Slot::Sym(s),
			Val::RFn(r) => Slot::RFn(r),
			Val::Arr(ref a) => Slot::Arr(Gc::from_root(a)),
			Val::Str(ref s) => Slot::Str(Gc::from_root(s)),
			Val::Tab(ref t) => Slot::Tab(Gc::from_root(t)),
			Val::GIter(ref g) => Slot::GIter(Gc::from_root(g)),
			Val::Obj(ref o) => Slot::Obj(Gc::from_root(o)),
			Val::Class(ref c) => Slot::Class(Gc::from_root(c)),
			Val::GFn(ref g) => Slot::GFn(Gc::from_root(g)),
			Val::Coro(ref c) => Slot::Coro(Gc::from_root(c)),
			Val::RData(ref r) => Slot::RData(Gc::from_root(r))
		}
	}

	pub(crate) fn root(&self) -> Val {
		match *self {
			Slot::Nil => Val::Nil,
			Slot::Int(i) => Val::Int(i),
			Slot::Char(c) => Val::Char(c),
			Slot::Flo(f) => Val::Flo(f),
			Slot::Bool(b) => Val::Bool(b),
			Slot::Sym(s) => Val::Sym(s),
			Slot::RFn(r) => Val::RFn(r),
			Slot::Arr(ref a) => Val::Arr(a.root()),
			Slot::Str(ref s) => Val::Str(s.root()),
			Slot::Tab(ref t) => Val::Tab(t.root()),
			Slot::GIter(ref g) => Val::GIter(g.root()),
			Slot::Obj(ref o) => Val::Obj(o.root()),
			Slot::Class(ref c) => Val::Class(c.root()),
			Slot::GFn(ref c) => Val::GFn(c.root()),
			Slot::Coro(ref c) => Val::Coro(c.root()),
			Slot::RData(ref r) => Val::RData(r.root()),
		}
	}

	pub(crate) fn into_root(self) -> Val {
		match self {
			Slot::Nil => Val::Nil,
			Slot::Int(i) => Val::Int(i),
			Slot::Char(c) => Val::Char(c),
			Slot::Flo(f) => Val::Flo(f),
			Slot::Bool(b) => Val::Bool(b),
			Slot::Sym(s) => Val::Sym(s),
			Slot::RFn(r) => Val::RFn(r),
			Slot::Arr(a) => Val::Arr(a.into_root()),
			Slot::Str(s) => Val::Str(s.into_root()),
			Slot::Tab(t) => Val::Tab(t.into_root()),
			Slot::GIter(g) => Val::GIter(g.into_root()),
			Slot::Obj(o) => Val::Obj(o.into_root()),
			Slot::Class(c) => Val::Class(c.into_root()),
			Slot::GFn(c) => Val::GFn(c.into_root()),
			Slot::Coro(c) => Val::Coro(c.into_root()),
			Slot::RData(r) => Val::RData(r.into_root()),
		}
	}

	pub fn type_name(&self) -> &'static str {
		self.root().type_name()
	}

	pub fn a_type_name(&self) -> &'static str {
		self.root().a_type_name()
	}
}

//Slot implements Eq and Hash so that it can be used as HashMap key. unlike Val, its PartialEq 
//implementation has the semantics of keys_eqv, rather than eq.
impl PartialEq<Slot> for Slot {
	fn eq(&self, other: &Slot) -> bool {
		self.root().keys_eqv(&other.root())
	}
}

impl Eq for Slot { }

impl Hash for Slot {
	fn hash<H: Hasher>(&self, state: &mut H) {
		match *self {
			Slot::Nil => Hashable(Val::Nil).hash(state),
			Slot::Int(i) => Hashable(Val::Int(i)).hash(state),
			Slot::Flo(f) => Hashable(Val::Flo(f)).hash(state),
			Slot::Char(c) => Hashable(Val::Char(c)).hash(state),
			Slot::Bool(b) => Hashable(Val::Bool(b)).hash(state),
			Slot::Sym(s) => Hashable(Val::Sym(s)).hash(state),
			Slot::RFn(f) => Hashable(Val::RFn(f)).hash(state),
			Slot::Arr(ref gc) => (**gc).hash(state),
			Slot::Str(ref gc) => (**gc).hash(state),
			Slot::Tab(ref gc) => (&**gc as *const _ as usize).hash(state),
			Slot::GIter(ref gc) => (&**gc as *const _ as usize).hash(state),
			Slot::Obj(ref gc) => (&**gc as *const _ as usize).hash(state),
			Slot::Class(ref gc) => (&**gc as *const _ as usize).hash(state),
			Slot::GFn(ref gc) => (&**gc as *const _ as usize).hash(state),
			Slot::Coro(ref gc) => (&**gc as *const _ as usize).hash(state),
			Slot::RData(ref gc) => (&**gc as *const _ as usize).hash(state)
		}
	}
}

//the GcHeader is made of two 32-bit words.

//the high word dedicates its topmost eight bits to the Engine id, and the next bit to a "frozen"
//flag. its lower 23 bits are an unsigned index into the roots vec (or 0x_007f_ffff when unrooted). 
//the root count is stored in the roots vec, rather than inline, to avoid wasting space in the 
//header of unrooted objects.

//the lower word is 0x_ffff_ffff for a marked young object or 0x_ffff_fffe for an unmarked young
//object. otherwise, the upper two bits are the color index, and the lower 30 bits are an unsigned 
//index into the old-objects vec for that color.

//the frozen flag isn't actually written or read by the gc at all. we just store it in the gc 
//header because otherwise there would be several structs with a `frozen: Cell<bool>` field,
//taking up 64 bits of storage for 1 bit of information.

const ENGINE_ID_SHIFT: u32 = 24;
const ENGINE_ID_MASK: u32 = 0xff << 24;
const FROZEN_BIT: u32 = 0x1 << 23;
const ROOT_INDEX_MASK: u32 = !(ENGINE_ID_MASK | FROZEN_BIT);
const UNROOTED_BITS: u32 = ROOT_INDEX_MASK;
const MAX_ROOT_INDEX: usize = (ROOT_INDEX_MASK - 1) as usize;

const MARKED_YOUNG_BITS: u32 = 0x_ffff_ffff;
const UNMARKED_YOUNG_BITS: u32 = 0x_ffff_fffe;
const COLOR_SHIFT: u32 = 30;
const COLOR_MASK: u32 = 0x3 << 30;
const OLD_INDEX_MASK: u32 = !COLOR_MASK;
const MAX_OLD_INDEX: usize = (OLD_INDEX_MASK - 2) as usize;

#[doc(hidden)]
#[derive(Clone)]
pub struct GcHeader {
	hi: Cell<u32>,
	lo: Cell<u32>
}

impl GcHeader {
	pub(crate) fn new() -> GcHeader {
		let engine_id = ACTIVE_ENGINE_ID.with(|id| id.get().unwrap()) as u32;
		GcHeader {
			hi: Cell::new((engine_id << ENGINE_ID_SHIFT) | UNROOTED_BITS),
			lo: Cell::new(UNMARKED_YOUNG_BITS)
		}
	}

	fn reset(&self) {
		let engine_id = ACTIVE_ENGINE_ID.with(|id| id.get().unwrap()) as u32;
		self.hi.set((engine_id << ENGINE_ID_SHIFT) | UNROOTED_BITS);
		self.lo.set(UNMARKED_YOUNG_BITS);
	}

	fn engine_id(&self) -> u8 {
		(self.hi.get() >> ENGINE_ID_SHIFT) as u8
	}

	pub(crate) fn frozen(&self) -> bool {
		(self.hi.get() & FROZEN_BIT) != 0
	}

	pub(crate) fn freeze(&self) {
		self.hi.set(self.hi.get() | FROZEN_BIT);
	}

	fn rooted(&self) -> bool {
		(self.hi.get() & ROOT_INDEX_MASK) != UNROOTED_BITS
	}

	fn unroot(&self) {
		self.hi.set(self.hi.get() | UNROOTED_BITS);
	}

	fn root_index(&self) -> usize {
		(self.hi.get() & ROOT_INDEX_MASK) as usize
	}

	fn set_root_index(&self, root_index: usize) {
		debug_assert!(root_index <= MAX_ROOT_INDEX);
		self.hi.set((self.hi.get() & !ROOT_INDEX_MASK) | (root_index as u32));
	}

	fn young(&self) -> bool {
		self.lo.get() >= UNMARKED_YOUNG_BITS
	}

	fn marked(&self) -> bool {
		debug_assert!(self.young());
		self.lo.get() == MARKED_YOUNG_BITS
	}

	fn mark(&self) {
		debug_assert!(self.young() && !self.marked());
		self.lo.set(MARKED_YOUNG_BITS)
	}

	fn promote(&self, color_index: usize, old_index: usize) {
		debug_assert!(self.young() && color_index <= 3 && old_index <= MAX_OLD_INDEX);
		self.lo.set(((color_index as u32) << COLOR_SHIFT) | (old_index as u32));
	}

	fn color_index(&self) -> usize {
		debug_assert!(!self.young());
		(self.lo.get() >> COLOR_SHIFT) as usize
	}

	fn set_color_index(&self, color_index: usize) {
		debug_assert!(!self.young());
		debug_assert!(color_index <= 3);
		self.lo.set((self.lo.get() & !COLOR_MASK) | ((color_index as u32) << COLOR_SHIFT));
	}

	fn old_index(&self) -> usize {
		debug_assert!(!self.young());
		(self.lo.get() & OLD_INDEX_MASK) as usize
	}

	fn set_old_index(&self, old_index: usize) {
		debug_assert!(!self.young() && old_index <= MAX_OLD_INDEX);
		self.lo.set((self.lo.get() & !OLD_INDEX_MASK) | (old_index as u32));
	}
}

#[doc(hidden)]
pub trait Visitor {
	fn visit_gc<T: Allocate>(&mut self, rc: &Gc<T>);

	fn visit_slot(&mut self, slot: &Slot) {
		match *slot {
			Slot::Nil | Slot::Int(_) | Slot::Char(_) | Slot::Flo(_) | 
			Slot::Bool(_) | Slot::Sym(_) | Slot::RFn(_) => (),
			Slot::Arr(ref a) => self.visit_gc(a),
			Slot::Str(ref s) => self.visit_gc(s),
			Slot::Tab(ref t) => self.visit_gc(t),
			Slot::GIter(ref g) => self.visit_gc(g),
			Slot::Obj(ref o) => self.visit_gc(o),
			Slot::Class(ref c) => self.visit_gc(c),
			Slot::GFn(ref g) => self.visit_gc(g),
			Slot::Coro(ref c) => self.visit_gc(c),
			Slot::RData(ref r) => self.visit_gc(r)
		}
	}
}

struct MarkingVisitor<'a, 'b> {
	heap: &'a Heap,
	marking_stack: &'b mut RefMut<'a, Vec<ErasedGc>>,
	old_objects: &'b mut [RefMut<'a, Vec<ErasedGc>>; 4],
	old_only: bool
}

impl<'a, 'b> MarkingVisitor<'a, 'b> {
	fn new(
		heap: &'a Heap,
		marking_stack: &'b mut RefMut<'a, Vec<ErasedGc>>,
		old_objects: &'b mut [RefMut<'a, Vec<ErasedGc>>; 4],
		old_only: bool
	) -> MarkingVisitor<'a, 'b> 
	{
		MarkingVisitor {
			heap,
			marking_stack,
			old_objects,
			old_only
		}
	}
}

impl<'a, 'b> Visitor for MarkingVisitor<'a, 'b> {
	fn visit_gc<T: Allocate>(&mut self, gc: &Gc<T>) {
		let header = gc.header();

		//if the target object is young and unmarked, mark it. if it's old and white, turn it gray.
		if header.young() {
			debug_assert!(!self.old_only);

			if !header.marked() {
				header.mark();
				self.marking_stack.push(T::erase_gc(gc.clone()));
			}
		} else {
			if header.color_index() == self.heap.white_index.get() {
				self.heap.change_color(gc, self.heap.gray_index.get(), self.old_objects);
			}
		}
	}
}

#[doc(hidden)]
pub trait Allocate: Sized + GStore + Erase {
	fn visit_gcs<V: Visitor>(&self, visitor: &mut V);

	fn clear_gcs(&self);

	//we have to make the GcHeader an intrusive field on the allocated type, because 
	//otherwise it would be impossible to write-barrier an Arr without holding a Gc<Arr>
	//or Root<Arr>
	fn header(&self) -> &GcHeader;

	//this method does not consider Self, but does include any heap allocations which are
	//exclusively owned by Self (a Box or a Vec would count, but a  Root or an Rc wouldn't). 
	//we've had to establish the memory usage of rust's std collections by peeking at their
	//source code; this information may become inaccurate as time passes.
	fn owned_memory_usage(&self) -> usize;

	fn memory_usage(&self) -> usize {
		size_of::<Self>() + self.owned_memory_usage()
	}
}

//the minimum number of old black bytes which must be present before a cycle can end
const MIN_SURVIVING_BYTES: usize = 1024 * 1024;

const INITIAL_U: f32 = 1.5;
const INITIAL_R: f32 = 2.0 / (INITIAL_U - 1.0);
const INITIAL_W: Option<f32> = None;

/** Equivalent to [`(gc-value 'min-ratio)`](https://gamelisp.rs/std/gc-value). */
pub const GC_MIN_RATIO: f32 = 1.2;

/** Equivalent to [`(gc-value 'default-ratio)`](https://gamelisp.rs/std/gc-value). */
pub const GC_DEFAULT_RATIO: f32 = INITIAL_U;

pub(crate) struct Heap {
	pub(crate) recycler: Recycler,

	young_objects: RefCell<Vec<ErasedGc>>,
	young_bytes: Cell<usize>,
	marking_stack: RefCell<Vec<ErasedGc>>,

	old_objects: [RefCell<Vec<ErasedGc>>; 4],
	old_bytes: [Cell<usize>; 4],
	white_index: Cell<usize>,
	gray_index: Cell<usize>,
	black_index: Cell<usize>,
	ghost_index: Cell<usize>,

	roots: RefCell<Vec<RootEntry>>,

	black_target: Cell<usize>,
	ghost_target: Cell<usize>,

	ratio_u: Cell<f32>,
	ratio_r: Cell<f32>,
	ratio_w: Cell<Option<f32>>,
}

impl Drop for Heap {
	fn drop(&mut self) {
		if self.roots.get_mut().len() > 0 {
			eprintln!("a Root has outlived its originating Runtime - aborting process");
			abort()
		}
	}
}

impl Heap {
	pub(crate) fn new() -> Heap {
		Heap {
			recycler: Recycler::new(),

			young_objects: RefCell::new(Vec::new()),
			young_bytes: Cell::new(0),
			marking_stack: RefCell::new(Vec::new()),

			old_objects: [RefCell::new(Vec::new()), RefCell::new(Vec::new()),
			              RefCell::new(Vec::new()), RefCell::new(Vec::new())],
			old_bytes: [Cell::new(0), Cell::new(0), Cell::new(0), Cell::new(0)],
			white_index: Cell::new(0),
			gray_index: Cell::new(1),
			black_index: Cell::new(2),
			ghost_index: Cell::new(3),

			roots: RefCell::new(Vec::new()),

			black_target: Cell::new(0),
			ghost_target: Cell::new(0),

			ratio_u: Cell::new(INITIAL_U),
			ratio_r: Cell::new(INITIAL_R),
			ratio_w: Cell::new(INITIAL_W)
		}
	}

	#[allow(dead_code)]
	pub(crate) fn clear(&self) {
		for erased in self.young_objects.borrow_mut().drain(..) {
			with_erased_gc!(erased, gc, gc.free())
		}

		self.young_bytes.set(0);
		self.marking_stack.borrow_mut().clear();

		for i in 0..4 {
			for erased in self.old_objects[i].borrow_mut().drain(..) {
				with_erased_gc!(erased, gc, gc.free())
			}

			self.old_bytes[i].set(0);
		}

		self.black_target.set(0);
		self.ghost_target.set(0);

		self.ratio_u.set(INITIAL_U);
		self.ratio_r.set(INITIAL_R);
		self.ratio_w.set(INITIAL_W);

		//we don't clear self.roots(), because we need it to check for extant Roots when the
		//Heap is dropped. in any case, Roots can't be stored on the Heap, so RootEntries can't 
		//cause a reference loop, so there's no need to clear them when Runtime is dropped.
	}

	pub(crate) fn ratio(&self) -> f32 {
		self.ratio_u.get()
	}

	pub(crate) fn set_ratio(&self, ratio: f32) {
		let ratio = f32::min(f32::max(GC_MIN_RATIO, ratio), 10.0);
		self.ratio_u.set(ratio);
		self.ratio_r.set(2.0 / (ratio - 1.0));
	}

	#[inline]
	pub(crate) fn alloc<T: Allocate>(&self, init: T) -> Root<T> {
		Root::new(self.alloc_gc(init))
	}

	pub(crate) fn alloc_gc<T: Allocate>(&self, init: T) -> Gc<T> {
		let gc = Gc::new(init);
		self.register_young(gc.clone());
		gc
	}

	fn register_young<T: Allocate>(&self, gc: Gc<T>) {
		let header = gc.header();
		debug_assert!(header.young() && !header.marked());

		self.young_bytes.set(self.young_bytes.get() + gc.memory_usage());
		self.young_objects.borrow_mut().push(T::erase_gc(gc));
	}

	fn promote<T: Allocate>(
		&self, 
		gc: &Gc<T>,
		old_objects: &mut [RefMut<Vec<ErasedGc>>; 4]
	) -> usize {
		debug_assert!(gc.header().young());

		let black_index = self.black_index.get();

		old_objects[black_index].push(T::erase_gc(gc.clone()));
		gc.header().promote(black_index, old_objects[black_index].len() - 1);

		let memory_usage = gc.memory_usage();
		self.old_bytes[black_index].set(self.old_bytes[black_index].get() + memory_usage);

		memory_usage
	}

	fn change_color<T: Allocate>(
		&self, 
		gc: &Gc<T>, 
		new_color_index: usize,
		old_objects: &mut [RefMut<Vec<ErasedGc>>; 4]
	) {
		let header = gc.header();
		debug_assert!(!header.young());

		let prev_color_index = header.color_index();
		debug_assert!(prev_color_index != new_color_index);

		//remove from the previous old_objects vec, using swap_remove. update the swapped object's
		//old_index to reflect its new position. this is similar to how we handle unrooting.
		let prev_i = header.old_index();
		let erased = old_objects[prev_color_index].swap_remove(prev_i);
		if prev_i < old_objects[prev_color_index].len() {
			old_objects[prev_color_index][prev_i].header().set_old_index(prev_i);
		}

		//push onto the destination old_objects vec and update our old_index
		old_objects[new_color_index].push(erased);
		header.set_old_index(old_objects[new_color_index].len() - 1);

		//update the object's color bits
		header.set_color_index(new_color_index);

		//update old_bytes
		let usage = gc.memory_usage();
		self.old_bytes[prev_color_index].set(self.old_bytes[prev_color_index].get() - usage);
		self.old_bytes[new_color_index].set(self.old_bytes[new_color_index].get() + usage);
	}

	//the caller is required to to write-barrier anything that's in the grey memory-areas (those 
	//which aren't write-barriered when mutated) just before calling collect_*.
	pub(crate) fn step(&self) {

		let mut young_objects = self.young_objects.borrow_mut();
		let mut old_objects = [
			self.old_objects[0].borrow_mut(),
			self.old_objects[1].borrow_mut(),
			self.old_objects[2].borrow_mut(),
			self.old_objects[3].borrow_mut()
		];
		let mut marking_stack = self.marking_stack.borrow_mut();

		let white_index = self.white_index.get();
		let gray_index = self.gray_index.get();
		let black_index = self.black_index.get();
		let ghost_index = self.ghost_index.get();

		//traverse all of the roots
		for root_entry in self.roots.borrow().iter() {
			with_erased_gc!(root_entry.gc, gc, {
				let mut visitor = MarkingVisitor::new(self, &mut marking_stack,
				                                      &mut old_objects, false);
				visitor.visit_gc(gc);
			})
		}

		//mark young objects: until the marking stack is empty, pop an object off it, mark
		//all of its young pointees and add them to the stack, and mark all of its old white
		//pointees as gray.
		while let Some(erased) = marking_stack.pop() {
			with_erased_gc!(erased, gc, {
				let mut visitor = MarkingVisitor::new(self, &mut marking_stack,
				                                      &mut old_objects, false);
				gc.visit_gcs(&mut visitor);
			})
		}

		//sweep young objects: empty the young generation. if a young object is unmarked, free it
		//immediately. if it's marked, promote it into an old black object.
		let mut promoted_bytes: usize = 0;

		for erased in young_objects.drain(..) {
			with_erased_gc!(erased, gc, {
				let header = gc.header();
				if header.marked() {
					promoted_bytes += self.promote(gc, &mut old_objects);
				} else {
					self.recycler.free(erased);
				}
			})
		}

		self.young_bytes.set(0);

		//the young collection is complete, so we move on to incrementally processing the
		//old white objects which survived the last cycle, and the ghost objects which didn't.

		//traverse (promoted_bytes * R) additional bytes of old gray objects, converting them into
		//old black objects.
		let target_incr = ((self.ratio_r.get() + 1.0) * promoted_bytes as f32).ceil() as usize;
		self.black_target.set(self.black_target.get() + target_incr);

		while self.old_bytes[black_index].get() < self.black_target.get() &&
		      !old_objects[gray_index].is_empty() {

			let erased = old_objects[gray_index].last().unwrap().clone();

			with_erased_gc!(erased, gc, {
				self.change_color(gc, black_index, &mut old_objects);

				let mut visitor = MarkingVisitor::new(self, &mut marking_stack,
				                                      &mut old_objects, true);
				gc.visit_gcs(&mut visitor);
			})
		}

		//if there were a nonzero number of ghost objects last cycle, ratio_w will be Some. 
		//free (promoted_bytes * W/U) bytes of ghost objects.
		if let Some(ratio_w) = self.ratio_w.get() {
			let bytes_to_free = (ratio_w * target_incr as f32).ceil() as usize;
			self.ghost_target.set(self.ghost_target.get().saturating_sub(bytes_to_free));

			while self.old_bytes[ghost_index].get() > self.ghost_target.get() {
				let erased = old_objects[ghost_index].pop().unwrap();

				//note that with "unsafe-internals" disabled, this may cause latency spikes by
				//suddenly freeing a tree of Rc references all at once. we could solve this by
				//splitting it into two incremental passes: clear_gcs() followed by deleting the 
				//ghost ErasedGc itself.
				with_erased_gc!(erased, gc, {
					let memory_usage = gc.memory_usage();
					self.old_bytes[ghost_index].set(self.old_bytes[ghost_index].get() - 
					                                memory_usage);

					gc.free();
				})
			}
		} else {
			debug_assert!(old_objects[ghost_index].is_empty());
		}

		//if there are no gray objects left, and if we've produced at least MIN_SURVIVING_BYTES
		//of old black objects, then we've reached the end of the cycle. make all white objects
		//into ghost objects, update W, and turn all black objects white.
		if old_objects[gray_index].is_empty() && 
		   self.old_bytes[black_index].get() >= MIN_SURVIVING_BYTES {

			//if there are any remaining ghost objects (unlikely unless the surviving heap has
			//sharply decreased in size), we need to change each object's color index so that 
			//they'll remain ghosts rather than turning black.
			while let Some(erased) = old_objects[ghost_index].pop() {
				old_objects[white_index].push(erased.clone());

				let header = erased.header();
				header.set_color_index(white_index);
				header.set_old_index(old_objects[white_index].len() - 1);
			}

			self.old_bytes[white_index].set(
				self.old_bytes[white_index].get() + self.old_bytes[ghost_index].get()
			);
			self.old_bytes[ghost_index].set(0);

			//otherwise, we just shuffle the meaning of the different color indexes. objects
			//which were previously white become ghosts; ghost objects (should be none) become
			//black; black objects become white; and gray objects are unchanged.
			let prev_white_index = self.white_index.get();
			let prev_black_index = self.black_index.get();
			let prev_ghost_index = self.ghost_index.get();

			self.white_index.set(prev_black_index);
			self.black_index.set(prev_ghost_index);
			self.ghost_index.set(prev_white_index);

			//reset our targets...
			self.ghost_target.set(self.old_bytes[self.ghost_index.get()].get());
			self.black_target.set(0);

			//finally, recalculate W
			let ghost_bytes = self.old_bytes[self.ghost_index.get()].get();
			if ghost_bytes > 0 {
				//in the case where there are very few surviving objects, we know that the next
				//cycle still won't end until MIN_SURVIVING_BYTES have been processed.
				let surviving_bytes = self.old_bytes[self.white_index.get()].get();
				let denominator = max(surviving_bytes, MIN_SURVIVING_BYTES);
				let w = (ghost_bytes as f32) / (denominator as f32);
				self.ratio_w.set(Some(w));
			} else {
				self.ratio_w.set(None);
			}
		}
	}

	pub(crate) fn young_memory_usage(&self) -> usize {
		self.young_bytes.get()
	}

	pub(crate) fn old_memory_usage(&self) -> usize {
		self.old_bytes[self.white_index.get()].get() + 
		self.old_bytes[self.gray_index.get()].get() +
		self.old_bytes[self.black_index.get()].get()
	}

	pub(crate) fn ghost_memory_usage(&self) -> usize {
		self.old_bytes[self.ghost_index.get()].get()
	}

	pub(crate) fn traverse_stack_slot(&self, dst: &Slot) {
		match *dst {
			Slot::Nil | Slot::Int(_) | Slot::Char(_) | Slot::Flo(_) | 
			Slot::Bool(_) | Slot::Sym(_) | Slot::RFn(_) => (),
			Slot::Arr(ref gc) => self.traverse_stack_gc(gc),
			Slot::Str(ref gc) => self.traverse_stack_gc(gc),
			Slot::Tab(ref gc) => self.traverse_stack_gc(gc),
			Slot::GIter(ref gc) => self.traverse_stack_gc(gc),
			Slot::Obj(ref gc) => self.traverse_stack_gc(gc),
			Slot::Class(ref gc) => self.traverse_stack_gc(gc),
			Slot::GFn(ref gc) => self.traverse_stack_gc(gc),
			Slot::Coro(ref gc) => self.traverse_stack_gc(gc),
			Slot::RData(ref gc) => self.traverse_stack_gc(gc)
		}
	}

	pub(crate) fn traverse_stack_gc<T: Allocate>(&self, dst: &Gc<T>) {
		let header = dst.header();

		//this should be impossible, but we check it for an extra level of security anyway
		let engine_id = ACTIVE_ENGINE_ID.with(|id| id.get().unwrap());
		if engine_id != header.engine_id() {
			eprintln!("attempted to move a Root to another Runtime - aborting process");
			abort()
		}

		//if dst is young, mark it. if it's old and white, turn it gray. this is copied from
		//write_barrier(), below.
		if header.young() {
			if !header.marked() {
				self.marking_stack.borrow_mut().push(T::erase_gc(dst.clone()));
				header.mark();
			}
		} else {
			if header.color_index() == self.white_index.get() {
				let mut old_objects = [
			   		self.old_objects[0].borrow_mut(),
			   		self.old_objects[1].borrow_mut(),
			   		self.old_objects[2].borrow_mut(),
			   		self.old_objects[3].borrow_mut()
			   	];

				self.change_color(dst, self.gray_index.get(), &mut old_objects);
			}
		}
	}

	pub(crate) fn write_barrier_val<T: Allocate>(&self, src: &T, dst: &Val) {
		match *dst {
			Val::Nil | Val::Int(_) | Val::Char(_) | Val::Flo(_) | 
			Val::Bool(_) | Val::Sym(_) | Val::RFn(_) => (),
			Val::Arr(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::Str(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::Tab(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::GIter(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::Obj(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::Class(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::GFn(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::Coro(ref root) => self.write_barrier(src, &root.to_gc()),
			Val::RData(ref root) => self.write_barrier(src, &root.to_gc())
		}
	}

	pub(crate) fn write_barrier_slot<T: Allocate>(&self, src: &T, dst: &Slot) {
		match *dst {
			Slot::Nil | Slot::Int(_) | Slot::Char(_) | Slot::Flo(_) | 
			Slot::Bool(_) | Slot::Sym(_) | Slot::RFn(_) => (),
			Slot::Arr(ref gc) => self.write_barrier(src, gc),
			Slot::Str(ref gc) => self.write_barrier(src, gc),
			Slot::Tab(ref gc) => self.write_barrier(src, gc),
			Slot::GIter(ref gc) => self.write_barrier(src, gc),
			Slot::Obj(ref gc) => self.write_barrier(src, gc),
			Slot::Class(ref gc) => self.write_barrier(src, gc),
			Slot::GFn(ref gc) => self.write_barrier(src, gc),
			Slot::Coro(ref gc) => self.write_barrier(src, gc),
			Slot::RData(ref gc) => self.write_barrier(src, gc)
		}
	}

	pub(crate) fn write_barrier<T, U>(&self, src: &T, dst: &Gc<U>)
	where
		T: Allocate,
		U: Allocate
	{	
		//we're limited here by the fact that src has to be &T rather than &Gc<T>. ideally we
		//would track old cross-references into the young generation via a pointer to src rather
		//than dst, so that we can re-traverse src at the start of the young collection, so that
		//young objects aren't being promoted to the old generation just because an old object
		//happened to point to them at least once during the previous frame.

		//i anticipate this should be fairly rare (most short-lived objects will only be pointed
		//to by stack variables or by other short-lived objects), so we can live with it for now.

		let src_header = src.header();
		let dst_header = dst.header();

		if src_header.engine_id() != dst_header.engine_id() {
			eprintln!("attempted to move a Root to another Runtime - aborting process");
			abort()
		}

		//if the destination is an unmarked young object being cross-referenced by an old object, 
		//we mark it. if it's a white object being pointed to by a black object, we turn it gray.
		if !src_header.young() {
			if dst_header.young() {
				if !dst_header.marked() {
					self.marking_stack.borrow_mut().push(U::erase_gc(dst.clone()));
					dst_header.mark();
				}
			} else {
				if dst_header.color_index() == self.white_index.get() &&
				   src_header.color_index() == self.black_index.get() {

				   	let mut old_objects = [
				   		self.old_objects[0].borrow_mut(),
				   		self.old_objects[1].borrow_mut(),
				   		self.old_objects[2].borrow_mut(),
				   		self.old_objects[3].borrow_mut()
				   	];

					self.change_color(dst, self.gray_index.get(), &mut old_objects);
				}
			}
		}
	}

	pub(crate) fn memory_usage_barrier<T>(&self, src: &T, prev_usage: usize, cur_usage: usize) 
	where
		T: Allocate
	{
		let header = src.header();

		if header.young() {
			let current = self.young_bytes.get();
			let new = (current + cur_usage) - prev_usage;
			self.young_bytes.set(new);
		} else {
			let i = header.color_index();

			let current = self.old_bytes[i].get();
			let new = (current + cur_usage) - prev_usage;
			self.old_bytes[i].set(new);
		}
	}
}


//-------------------------------------------------------------------------------------------------
// Recycler
//-------------------------------------------------------------------------------------------------

/*
when the gc is under load, most of its time is spent freeing unmarked young allocations 
(effectively just calling Box::drop in a tight loop).

the vast majority of garbage data in tcof is Arrs, with a small amount being Stays and GFns. for
consistent performance, we should also handle programs which generate large amounts of garbage 
Strs, Tabs, Obj, Coros or GIters.
	
	[todo: handle those additional types, but not yet. recycling is a fairly large maintenance 
	burden and i want APIs to stabilise first.]

in order to reduce the pressure on the allocator, we maintain a free list for small instances
of these types. in tcof, this increases total memory usage only slightly, while more than 
doubling the gc's performance.
*/

const MAX_ARR_CAPACITY: usize = 16;

pub(crate) struct Recycler {
	arrs: Vec<RefCell<Vec<Gc<Arr>>>>,
	giters: RefCell<Vec<Gc<GIter>>>
}

impl Recycler {
	fn new() -> Recycler {
		Recycler {
			arrs: vec![RefCell::new(Vec::new()); 32],
			giters: RefCell::new(Vec::new())
		}
	}

	fn free(&self, erased: ErasedGc) {
		match erased {
			ErasedGc::Arr(arr) => {
				let cap = arr.capacity();
				if cap < MAX_ARR_CAPACITY {
					let header = arr.header();
					debug_assert!(!header.rooted());
					header.reset();

					arr.clear().unwrap();
					arr.set_span(Span::default());

					self.arrs[cap].borrow_mut().push(arr);
				} else {
					arr.free();
				}
			}
			ErasedGc::GIter(giter) => {
				let header = giter.header();
				debug_assert!(!header.rooted());
				header.reset();

				giter.clear_gcs();
				self.giters.borrow_mut().push(giter);
			}
			ErasedGc::Str(gc) => gc.free(),
			ErasedGc::Tab(gc) => gc.free(),
			ErasedGc::Obj(gc) => gc.free(),
			ErasedGc::Class(gc) => gc.free(),
			ErasedGc::GFn(gc) => gc.free(),
			ErasedGc::Stay(gc) => gc.free(),
			ErasedGc::Coro(gc) => gc.free(),
			ErasedGc::RData(gc) => gc.free(),
			ErasedGc::Bytecode(gc) => gc.free(),
			ErasedGc::Lambda(gc) => gc.free()
		}
	}

	pub(crate) fn arr(&self) -> Root<Arr> {
		for arrs in &self.arrs {
			let mut arrs = arrs.borrow_mut();
			if !arrs.is_empty() {
				let arr = arrs.pop().unwrap().into_root();
				drop(arrs);

				with_heap(|heap| heap.register_young(arr.to_gc()));
				return arr
			}
		}

		glsp::alloc(Arr::new())
	}

	pub(crate) fn arr_with_capacity(&self, capacity: usize) -> Root<Arr> {
		for i in capacity .. MAX_ARR_CAPACITY {
			let mut arrs = self.arrs[i].borrow_mut();
			if !arrs.is_empty() {
				let arr = arrs.pop().unwrap().into_root();
				drop(arrs);

				with_heap(|heap| heap.register_young(arr.to_gc()));
				return arr
			}
		}

		glsp::alloc(Arr::with_capacity(capacity))
	}

	pub(crate) fn arr_from_elem<V: ToVal>(
		&self, 
		elem: V, 
		reps: usize
	) -> GResult<Root<Arr>> {
		for i in reps .. MAX_ARR_CAPACITY {
			let mut arrs = self.arrs[i].borrow_mut();
			if !arrs.is_empty() {
				let arr = arrs.pop().unwrap().into_root();
				drop(arrs);

				with_heap(|heap| heap.register_young(arr.to_gc()));

				for _ in 0 .. reps {
					arr.push(elem.to_slot()?)?;
				}

				return Ok(arr)
			}
		}

		Ok(glsp::alloc(Arr::from_elem(elem, reps)?))
	}

	pub(crate) fn arr_from_iter<T, V>(&self, source: T) -> GResult<Root<Arr>> 
	where
		T: IntoIterator<Item = V>,
		V: ToVal
	{
		let iter = source.into_iter();
		let (min_size, max_size) = iter.size_hint();

		if max_size.is_some() && min_size == max_size.unwrap() {
			let capacity = min_size;
			for i in capacity .. MAX_ARR_CAPACITY {
				let mut arrs = self.arrs[i].borrow_mut();
				if !arrs.is_empty() {
					let arr = arrs.pop().unwrap().into_root();
					drop(arrs);

					with_heap(|heap| heap.register_young(arr.to_gc()));

					for item in iter {
						arr.push(item.to_slot()?)?;
					}

					return Ok(arr)
				}
			}
		}

		Ok(glsp::alloc(Arr::from_iter(iter)?))
	}

	pub(crate) fn giter(&self, state: GIterState) -> Root<GIter> {
		if let Some(gc) = self.giters.borrow_mut().pop() {
			let giter = gc.root();
			with_heap(|heap| heap.register_young(gc));

			//todo: do we need to write-barrier here...? the write barrier doesn't currently
			//do anything if `src` is in the young generation, but that could change in
			//the future.
			*giter.state.borrow_mut() = state;

			giter
		} else {
			glsp::alloc(GIter::new(state))
		}
	}
}
