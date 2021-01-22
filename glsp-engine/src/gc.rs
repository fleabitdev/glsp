use super::class::{Class, Obj};
use super::code::{Bytecode, Coro, GFn, Lambda, Stay};
use super::collections::{Arr, DequeOps, Str, Tab};
use super::engine::{glsp, with_heap, Guard, RData, RFn, RGc, Span, Sym};
use super::error::GResult;
use super::iter::{GIter, GIterState};
use super::val::{Hashable, Val};
use super::wrap::IntoVal;
use std::borrow::Borrow;
use std::cell::{Cell, RefCell, RefMut};
use std::cmp::{max, min, Ordering};
use std::f32;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::Deref;
use std::process::abort;

//the garbage collector currently uses a hybrid incremental and generational algorithm. see
//notes/gc.md for the details.

//-------------------------------------------------------------------------------------------------
// Raw
//-------------------------------------------------------------------------------------------------

// safe Raw<T> implementation
//----------------------------------------------------------------------------

#[cfg(not(feature = "unsafe-internals"))]
use std::rc::Rc;

#[doc(hidden)]
#[cfg(not(feature = "unsafe-internals"))]
pub struct Raw<T: Allocate> {
    rc: Rc<T>,
}

#[cfg(not(feature = "unsafe-internals"))]
impl<T: Allocate> Raw<T> {
    #[inline]
    fn new(t: T) -> Raw<T> {
        Raw { rc: Rc::new(t) }
    }

    #[inline]
    pub(crate) fn ptr_eq(raw0: &Raw<T>, raw1: &Raw<T>) -> bool {
        Rc::ptr_eq(&raw0.rc, &raw1.rc)
    }

    #[inline]
    fn as_usize(&self) -> usize {
        &(*self.rc) as *const T as usize
    }

    #[inline]
    fn free(&self) {
        self.clear_raws()
    }
}

#[cfg(not(feature = "unsafe-internals"))]
impl<T: Allocate> Clone for Raw<T> {
    #[inline]
    fn clone(&self) -> Raw<T> {
        Raw {
            rc: Rc::clone(&self.rc),
        }
    }
}

#[cfg(not(feature = "unsafe-internals"))]
impl<T: Allocate> Deref for Raw<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &*self.rc
    }
}

// unsafe Raw<T> implementation
//----------------------------------------------------------------------------

#[cfg(feature = "unsafe-internals")]
use std::ptr::NonNull;

#[doc(hidden)]
#[cfg(feature = "unsafe-internals")]
pub struct Raw<T: Allocate> {
    ptr: NonNull<T>,
}

#[cfg(feature = "unsafe-internals")]
impl<T: Allocate> Raw<T> {
    #[inline]
    fn new(t: T) -> Raw<T> {
        Raw {
            ptr: NonNull::new(Box::into_raw(Box::new(t))).unwrap(),
        }
    }

    #[inline]
    pub(crate) fn ptr_eq(raw0: &Raw<T>, raw1: &Raw<T>) -> bool {
        raw0.ptr == raw1.ptr
    }

    #[inline]
    fn as_usize(&self) -> usize {
        self.ptr.as_ptr() as usize
    }

    #[inline]
    fn free(&self) {
        unsafe { drop(Box::from_raw(self.ptr.as_ptr())) }
    }
}

#[cfg(feature = "unsafe-internals")]
impl<T: Allocate> Clone for Raw<T> {
    #[inline]
    fn clone(&self) -> Raw<T> {
        Raw { ptr: self.ptr }
    }
}

#[cfg(feature = "unsafe-internals")]
impl<T: Allocate> Deref for Raw<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        unsafe { &*self.ptr.as_ptr() }
    }
}

// common Raw<T> methods
//----------------------------------------------------------------------------

impl<T: Allocate> Raw<T> {
    #[inline]
    pub(crate) fn from_root(root: &Root<T>) -> Raw<T> {
        let engine_id = with_heap(|heap| heap.engine_id);
        if engine_id != root.header().engine_id() {
            eprintln!("attempted to move a Root to another Runtime - aborting process");
            abort()
        }

        root.raw.clone()
    }

    #[inline]
    pub(crate) fn root(&self) -> Root<T> {
        Root::new(self.clone())
    }

    #[inline]
    pub(crate) fn into_root(self) -> Root<T> {
        Root::new(self)
    }

    #[inline]
    fn header(&self) -> &Header {
        (**self).header()
    }
}

impl<T: Allocate> PartialEq for Raw<T> {
    #[inline]
    fn eq(&self, other: &Raw<T>) -> bool {
        self.as_usize() == other.as_usize()
    }
}

impl<T: Allocate> Eq for Raw<T> {}

impl<T: Allocate> Hash for Raw<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_usize().hash(state)
    }
}

//-------------------------------------------------------------------------------------------------
// ErasedRaw
//-------------------------------------------------------------------------------------------------

macro_rules! erased_types {
    ($($type_name:ident),+) => (

        #[doc(hidden)]
        pub trait Erase {
            fn erase_raw(raw: Raw<Self>) -> ErasedRaw where Self: Allocate;
            fn unerase_raw(erased_raw: &ErasedRaw) -> Raw<Self> where Self: Allocate;
        }

        $(impl Erase for $type_name {
            #[inline(always)]
            fn erase_raw(raw: Raw<$type_name>) -> ErasedRaw {
                ErasedRaw::$type_name(raw)
            }

            #[inline(always)]
            fn unerase_raw(erased_raw: &ErasedRaw) -> Raw<Self> {
                match erased_raw {
                    ErasedRaw::$type_name(raw) => raw.clone(),
                    _ => panic!()
                }
            }
        })+

        #[doc(hidden)]
        #[derive(Clone)]
        pub enum ErasedRaw {
            $($type_name(Raw<$type_name>)),+
        }

        impl ErasedRaw {
            fn header(&self) -> &Header {
                match *self {
                    $(ErasedRaw::$type_name(ref raw) => &raw.header()),+
                }
            }
        }

        impl Debug for ErasedRaw {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    $(
                        ErasedRaw::$type_name(ref raw) => {
                            write!(f, "ErasedRaw::{}(0x{:x})", stringify!($type_name),
                                   (&**raw) as *const $type_name as usize)
                        }
                    )+
                }
            }
        }

        macro_rules! with_erased_raw {
            ($erased:expr, $raw_ident:ident, $body:expr) => (
                match $erased {
                    $(ErasedRaw::$type_name(ref $raw_ident) => $body),+
                }
            );
        }
    );
}

erased_types!(Arr, Str, Tab, GIter, Obj, Class, GFn, Stay, Coro, RData, RFn, Bytecode, Lambda);

//-------------------------------------------------------------------------------------------------
// Root, RootStorage
//-------------------------------------------------------------------------------------------------

/**
A pointer onto the garbage-collected heap.
*/

pub struct Root<T: Allocate> {
    pub(crate) raw: Raw<T>,
}

/*
i was tempted to align RootEntry to a power-of-two size to make indexing cheaper, but
i suspect the increased memory/cache pressure wouldn't be worth it.
*/

#[derive(Debug)]
struct RootEntry {
    raw: Option<ErasedRaw>,
    strong_count: u32,
    weak_count: u32,

    //null next/previous indexes are represented by 0
    next_entry: u32,
    prev_entry: u32,
}

const NULL_ENTRY: u32 = 0;

/*
RootStorage maintains three collections in a single vec:
    - a singly-linked list of vacant entries (strong_count = 0, weak_count = 0, raw = None)
    - a doubly-linked list of strong entries (strong_count > 0, raw = Some(_))
    - a non-iterable collection of weak entries (strong_count = 0, weak_count > 0, raw = ?)

for vacant entries, next_entry contains junk data; for weak entries, both next_entry and
prev_entry contain junk data. entry 0 is a placeholder, and its next_entry and prev_entry
are always junk data - this eliminates a lot of branches.

the storage vector grows in power-of-two increments, filling in any empty space with new
vacant entries.

the main purpose of the linked lists is to keep root indexes stable (because Gc pointers
actually store the root index, rather than pointing to the object itself).

we considered some clever tricks to try to improve iteration performance (e.g. switching between
vector and linked-list iteration depending on the vector's occupancy percentage, or reordering
the linked lists during vector iteration so that they're sequential in memory), but that would
add a lot of complexity for something which is unlikely to cost more than 1 to 10 nanoseconds per
Root per gc step. 10,000 Roots implies 0.01 to 0.1ms per gc step. probably not worth it.
*/

pub(crate) struct RootStorage {
    entries: Vec<RootEntry>,

    first_vacant: u32,
    first_strong: u32, /*
                       we could also add a last_strong field here, for appending new items to the end of the
                       strong list rather than its beginning. the only benefit would be that iteration through
                       the strong list would then generally move from lower memory addresses to higher ones, but
                       i'm pretty sure this wouldn't actually be more cache-friendly. it would also make alloc(),
                       increment_strong_count() and decrement_strong_count() more expensive... seems like
                       a bad trade-off.
                       */
}

impl RootStorage {
    fn new() -> RootStorage {
        let mut entries = Vec::with_capacity(128);
        while entries.len() < entries.capacity() {
            let next_entry = (entries.len() + 1) as u32;
            entries.push(RootEntry {
                raw: None,
                strong_count: 0,
                weak_count: 0,

                next_entry,
                prev_entry: 0xffff_ffff, //deliberate junk data
            });
        }

        RootStorage {
            entries,

            first_vacant: 1,
            first_strong: NULL_ENTRY,
        }
    }

    //double the length of `entries` by appending `entries.len()` new vacant entries
    #[inline(never)]
    fn grow(&mut self) {
        //checking against MAX_ROOT_INDEX here means that we don't need to check it
        //on every call to alloc()
        let new_len = min(MAX_ROOT_INDEX as usize + 1, self.entries.len() * 2);
        assert!(
            new_len > self.entries.len(),
            "no more than {} objects may be simultaneously rooted",
            MAX_ROOT_INDEX as usize + 1
        );

        let to_reserve = new_len - self.entries.len();
        self.entries.reserve_exact(to_reserve);

        while self.entries.len() < self.entries.capacity() {
            let next_entry = (self.entries.len() + 1) as u32;
            self.entries.push(RootEntry {
                raw: None,
                strong_count: 0,
                weak_count: 0,

                next_entry,
                prev_entry: 0xffff_ffff, //deliberate junk data
            });
        }
    }

    //change the first vacant entry into a strong entry with a strong_count of 1 and the
    //specified `raw`, then return its index
    #[inline(always)]
    fn alloc(&mut self, raw: ErasedRaw) -> u32 {
        //when we're at full capacity, we set first_vacant to the index just after the end of the
        //vec. this should elide one of the bounds-checks below, because we've already asserted
        //that first_vacant falls within the vec's bounds.
        debug_assert!(self.first_vacant != NULL_ENTRY);

        if self.first_vacant as usize >= self.entries.len() {
            self.grow();
            self.alloc(raw)
        } else {
            let entry_index = self.first_vacant;

            let first_strong = self.first_strong;
            self.entries[first_strong as usize].prev_entry = entry_index;
            self.first_strong = entry_index;

            let entry = &mut self.entries[entry_index as usize];
            self.first_vacant = entry.next_entry;

            debug_assert!(entry.raw.is_none());
            debug_assert!(entry.strong_count == 0);
            debug_assert!(entry.weak_count == 0);

            entry.raw = Some(raw);
            entry.prev_entry = NULL_ENTRY;
            entry.next_entry = first_strong;

            entry.strong_count = 1;

            entry_index
        }
    }

    //increment the strong_count of the specified *weak* entry with a Some `raw` field,
    //changing it into a strong entry. (the strong_count of a strong entry can just be
    //incremented directly rather than calling this method.)
    #[inline(always)]
    fn increment_strong_count(&mut self, entry_index: u32) {
        debug_assert!(entry_index != NULL_ENTRY);

        let entry = &mut self.entries[entry_index as usize];

        debug_assert!(entry.raw.is_some());
        debug_assert!(entry.strong_count == 0);
        debug_assert!(entry.weak_count > 0);

        entry.prev_entry = NULL_ENTRY;
        entry.next_entry = self.first_strong;

        entry.strong_count = 1;
        drop(entry);

        self.entries[self.first_strong as usize].prev_entry = entry_index;
        self.first_strong = entry_index;
    }

    //decrement the strong_count of the specified strong entry. this may change it into a
    //weak entry or a vacant entry. returns `entry_index` if either the weak or strong
    //count are now greater than 0, or returns 0 otherwise.
    #[inline(always)]
    fn decrement_strong_count(&mut self, entry_index: u32) -> u32 {
        debug_assert!(entry_index != NULL_ENTRY);

        let entry = &mut self.entries[entry_index as usize];

        debug_assert!(entry.raw.is_some());
        debug_assert!(entry.strong_count >= 1);

        entry.strong_count -= 1;
        if entry.strong_count == 0 {
            //remove this entry from the strong list
            let prev_entry = entry.prev_entry;
            let next_entry = entry.next_entry;
            drop(entry); //end of borrow

            self.entries[prev_entry as usize].next_entry = next_entry;
            self.entries[next_entry as usize].prev_entry = prev_entry;

            //this branch could be fairly unpredictable, but it should get optimized to a cmov
            if prev_entry == NULL_ENTRY {
                self.first_strong = next_entry;
            }

            //potentially add this entry to the vacant list
            let entry = &mut self.entries[entry_index as usize];
            if entry.weak_count == 0 {
                entry.raw = None;

                entry.next_entry = self.first_vacant;
                self.first_vacant = entry_index;

                0
            } else {
                entry_index
            }
        } else {
            entry_index
        }
    }

    //decrement the weak_count of the specified weak or strong entry. this may change
    //a weak entry into a vacant entry. sets the pointee's header root_index to 0 if it
    //hasn't been deallocated, and both the weak and strong count have been reduced to 0.
    #[inline(always)]
    fn decrement_weak_count(&mut self, entry_index: u32) {
        debug_assert!(entry_index != NULL_ENTRY);

        let entry = &mut self.entries[entry_index as usize];

        debug_assert!(entry.weak_count >= 1);
        entry.weak_count -= 1;

        if entry.strong_count == 0 && entry.weak_count == 0 {
            if let Some(raw) = entry.raw.take() {
                raw.header().set_root_index(0);
            }

            entry.next_entry = self.first_vacant;
            self.first_vacant = entry_index;
        }
    }
}

impl<T: Allocate> Root<T> {
    #[inline]
    fn new(raw: Raw<T>) -> Root<T> {
        with_heap(|heap| {
            let header = raw.header();

            if heap.engine_id != header.engine_id() {
                eprintln!("attempted to create a Root for an inactive Runtime - aborting process");
                abort()
            }

            let mut root_storage = heap.root_storage.borrow_mut();

            let root_index = header.root_index();
            if root_index == 0 {
                header.set_root_index(root_storage.alloc(T::erase_raw(raw.clone())));
            } else {
                root_storage.entries[root_index as usize].strong_count += 1;
            }
        });

        Root { raw }
    }

    #[inline]
    pub(crate) fn as_raw(&self) -> &Raw<T> {
        &self.raw
    }

    #[inline]
    pub(crate) fn to_raw(&self) -> Raw<T> {
        Raw::from_root(self)
    }

    //weirdly, there's actually no way to do this safely in current rust. however, there's
    //no point special-casing it for "unsafe-internals" mode, because Raw::from_root is
    //basically free in that mode
    pub(crate) fn into_raw(self) -> Raw<T> {
        Raw::from_root(&self)
    }

    #[inline]
    pub fn ptr_eq(root0: &Root<T>, root1: &Root<T>) -> bool {
        Raw::ptr_eq(&root0.raw, &root1.raw)
    }
}

impl<T: Allocate> Clone for Root<T> {
    #[inline]
    fn clone(&self) -> Root<T> {
        with_heap(|heap| {
            let header = self.raw.header();
            if heap.engine_id != header.engine_id() {
                eprintln!("attempted to clone a Root for an inactive Runtime - aborting process");
                abort()
            }

            let root_index = header.root_index();
            heap.root_storage.borrow_mut().entries[root_index as usize].strong_count += 1;
        });

        Root {
            raw: self.raw.clone(),
        }
    }
}

impl<T: Allocate> Drop for Root<T> {
    #[inline]
    fn drop(&mut self) {
        with_heap(|heap| {
            let header = self.raw.header();

            if heap.engine_id != header.engine_id() {
                eprintln!("attempted to drop a Root for an inactive Runtime - aborting process");
                abort()
            }

            let mut root_storage = heap.root_storage.borrow_mut();
            header.set_root_index(root_storage.decrement_strong_count(header.root_index()));
        });
    }
}

impl<T: Allocate> Borrow<T> for Root<T> {
    #[inline]
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: Allocate> AsRef<T> for Root<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: Allocate + PartialEq<T>> PartialEq<Root<T>> for Root<T> {
    #[inline]
    fn eq(&self, other: &Root<T>) -> bool {
        (**self).eq(&**other)
    }
}

impl<T: Allocate + Eq> Eq for Root<T> {}

impl<T: Allocate + PartialOrd<T>> PartialOrd<Root<T>> for Root<T> {
    #[inline]
    fn partial_cmp(&self, other: &Root<T>) -> Option<Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: Allocate + Ord> Ord for Root<T> {
    #[inline]
    fn cmp(&self, other: &Root<T>) -> Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: Allocate> Deref for Root<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.raw
    }
}

//-------------------------------------------------------------------------------------------------
// Gc
//-------------------------------------------------------------------------------------------------

/**
A weak pointer onto the garbage-collected heap.

`Gc`'s API is very similar to [`std::rc::Weak`]. You can construct a `Gc` by calling
[`Root::downgrade`](struct.Root.html#method.downgrade). A `Gc` can't be dereferenced; instead,
you should promote it to a `Root` by calling [`Gc::upgrade`](#method.upgrade).

[`std::rc::Weak`]: https://doc.rust-lang.org/std/rc/struct.Weak.html

You should almost always use [`Root`](struct.Root.html) rather than `Gc`.

The only exception is [`RData`](struct.RData.html). If you store a `Root` within an `RData`, it
will cause a memory leak. If you need to construct an `RData` which stores a pointer to another
heap-allocated object, you should:

- Use `Gc` where you would normally use `Root`.

- Call [`RClassBuilder::trace`](struct.RClassBuilder.html#method.trace) before constructing
  the `RData`.

- Call [`glsp::write_barrier`](fn.write_barrier.html) when the `RData` is mutated.

The last two steps are necessary to prevent the pointed-to object from being deallocated.

See also [`GcVal`](struct.GcVal.html) (a `Val` which stores `Gc` pointers rather than `Root`
pointers), and [`RGc`](struct.RGc.html) (a strongly-typed alternative to `Gc<RData>`).

    struct Collider {
        bounds: Rect,
        obj: Gc<Obj>
    }

    impl Collider {
        fn trace(&self, visitor: &mut GcVisitor) {
            visitor.visit(&self.obj);
        }
    }

    fn setup() {
        RClassBuilder::<Collider>::new()
            .trace(Collider::trace)
            .build();
    }

    fn new_collider(obj: Root<Obj>) -> GResult<Root<RData>> {
        let collider = Collider {
            bounds: obj.get("bounds")?,
            obj: obj.downgrade()
        };

        Ok(glsp::rdata(collider))
    }
*/

//each Gc stores a u32. the upper 8 bits are the engine_id, the lower 23 bits are the root_index
pub struct Gc<T: Allocate>(u32, PhantomData<*mut T>);

impl<T: Allocate> Root<T> {
    /**
    Constructs a new [weak pointer](struct.Gc.html) to this heap-allocated object.
    */
    #[inline]
    pub fn downgrade(&self) -> Gc<T> {
        with_heap(|heap| {
            let header = self.raw.header();

            if heap.engine_id != header.engine_id() {
                eprintln!("attempted to create a Gc for an inactive Runtime - aborting process");
                abort()
            }

            let root_index = header.root_index();
            debug_assert!(root_index != 0);

            heap.root_storage.borrow_mut().entries[root_index as usize].weak_count += 1;

            Gc(root_index | ((heap.engine_id as u32) << 24), PhantomData)
        })
    }
}

impl<T: Allocate> Gc<T> {
    #[inline]
    fn engine_id(&self) -> u8 {
        (self.0 >> 24) as u8
    }

    #[inline]
    fn root_index(&self) -> u32 {
        self.0 & 0x7fffff
    }

    /**
    Attempts to construct a [strong pointer](struct.Root.html) from this weak pointer.

    Returns `None` if the pointed-to object has been deallocated by the garbage
    collector. To prevent this, use [`glsp::write_barrier`](fn.write_barrier.html) and
    [`RClassBuilder::trace`](struct.RClassBuilder.html#method.trace).
    */
    #[inline]
    pub fn upgrade(&self) -> Option<Root<T>> {
        with_heap(|heap| {
            let mut root_storage = heap.root_storage.borrow_mut();
            let entry = &mut root_storage.entries[self.root_index() as usize];

            match entry.raw.as_ref() {
                Some(erased_raw) => {
                    let raw = T::unerase_raw(erased_raw);

                    if heap.engine_id != raw.header().engine_id() {
                        eprintln!(
                            "attempted to create a Root for an inactive \
                            Runtime - aborting process"
                        );
                        abort()
                    }

                    let header = raw.header();
                    if !header.young() && header.color_index() == heap.ghost_index.get() {
                        None
                    } else {
                        if entry.strong_count == 0 {
                            root_storage.increment_strong_count(self.root_index());
                        } else {
                            entry.strong_count += 1;
                        }

                        Some(Root { raw })
                    }
                }
                None => None,
            }
        })
    }

    /**
    Returns `true` if both `Gc`s point to the same heap-allocated object.
    */
    #[inline]
    pub fn ptr_eq(gc0: &Gc<T>, gc1: &Gc<T>) -> bool {
        gc0.0 == gc1.0
    }
}

impl<T: Allocate> Clone for Gc<T> {
    #[inline]
    fn clone(&self) -> Gc<T> {
        with_heap(|heap| {
            if heap.engine_id != self.engine_id() {
                eprintln!("attempted to clone a Gc for an inactive Runtime - aborting process");
                abort()
            }

            let mut root_storage = heap.root_storage.borrow_mut();
            let entry = &mut root_storage.entries[self.root_index() as usize];
            debug_assert!(entry.weak_count > 0);
            entry.weak_count += 1;

            Gc(self.0, PhantomData)
        })
    }
}

impl<T: Allocate> Drop for Gc<T> {
    #[inline]
    fn drop(&mut self) {
        with_heap(|heap| {
            if heap.engine_id != self.engine_id() {
                eprintln!("attempted to drop a Gc for an inactive Runtime - aborting process");
                abort()
            }

            heap.root_storage
                .borrow_mut()
                .decrement_weak_count(self.root_index());
        });
    }
}

//-------------------------------------------------------------------------------------------------
// GcVal
//-------------------------------------------------------------------------------------------------

/**
Equivalent to [`Val`](enum.Val.html), except that it stores weak [`Gc`](struct.Gc.html)
pointers rather than strong [`Root`](struct.Root.html) pointers.

Construct a `GcVal` by calling [`Val::downgrade`](enum.Val.html#method.downgrade). The `GcVal`
itself is an opaque struct which can't do anything useful. Instead, convert it to a `Val`
by calling its [`upgrade`](#method.upgrade) method.

A `GcVal` is only useful when you need to store a GameLisp value in an `RData`. `Val`s may
contain `Root`s, and storing a `Root` in an `RData` would cause a memory leak.
*/

#[derive(Clone)]
pub struct GcVal(GcValPriv);

#[derive(Clone)]
enum GcValPriv {
    Nil,
    Int(i32),
    Flo(f32),
    Char(char),
    Bool(bool),
    Sym(Sym),
    Arr(Gc<Arr>),
    Str(Gc<Str>),
    Tab(Gc<Tab>),
    GIter(Gc<GIter>),
    Obj(Gc<Obj>),
    Class(Gc<Class>),
    GFn(Gc<GFn>),
    Coro(Gc<Coro>),
    RData(Gc<RData>),
    RFn(Gc<RFn>),
}

impl Val {
    /**
    Constructs a [`GcVal`](struct.GcVal.html) based on this `Val`.
    */
    pub fn downgrade(&self) -> GcVal {
        GcVal(match self {
            Val::Nil => GcValPriv::Nil,
            Val::Int(i) => GcValPriv::Int(*i),
            Val::Char(c) => GcValPriv::Char(*c),
            Val::Flo(f) => GcValPriv::Flo(*f),
            Val::Bool(b) => GcValPriv::Bool(*b),
            Val::Sym(s) => GcValPriv::Sym(*s),
            Val::Arr(ref a) => GcValPriv::Arr(a.downgrade()),
            Val::Str(ref s) => GcValPriv::Str(s.downgrade()),
            Val::Tab(ref t) => GcValPriv::Tab(t.downgrade()),
            Val::GIter(ref g) => GcValPriv::GIter(g.downgrade()),
            Val::Obj(ref o) => GcValPriv::Obj(o.downgrade()),
            Val::Class(ref c) => GcValPriv::Class(c.downgrade()),
            Val::GFn(ref g) => GcValPriv::GFn(g.downgrade()),
            Val::Coro(ref c) => GcValPriv::Coro(c.downgrade()),
            Val::RData(ref r) => GcValPriv::RData(r.downgrade()),
            Val::RFn(ref r) => GcValPriv::RFn(r.downgrade()),
        })
    }
}

impl GcVal {
    /**
    Attempts to construct a [`Val`](enum.Val.html) based on this `GcVal`.

    Returns `None` if this `GcVal` points to a heap-allocated object which has been
    deallocated by the garbage collector. To prevent this, use
    [`glsp::write_barrier`](fn.write_barrier.html) and
    [`RClassBuilder::trace`](struct.RClassBuilder.html#method.trace).
    */
    pub fn upgrade(&self) -> Option<Val> {
        match &self.0 {
            GcValPriv::Nil => Some(Val::Nil),
            GcValPriv::Int(i) => Some(Val::Int(*i)),
            GcValPriv::Char(c) => Some(Val::Char(*c)),
            GcValPriv::Flo(f) => Some(Val::Flo(*f)),
            GcValPriv::Bool(b) => Some(Val::Bool(*b)),
            GcValPriv::Sym(s) => Some(Val::Sym(*s)),
            GcValPriv::Arr(ref a) => a.upgrade().map(Val::Arr),
            GcValPriv::Str(ref s) => s.upgrade().map(Val::Str),
            GcValPriv::Tab(ref t) => t.upgrade().map(Val::Tab),
            GcValPriv::GIter(ref g) => g.upgrade().map(Val::GIter),
            GcValPriv::Obj(ref o) => o.upgrade().map(Val::Obj),
            GcValPriv::Class(ref c) => c.upgrade().map(Val::Class),
            GcValPriv::GFn(ref g) => g.upgrade().map(Val::GFn),
            GcValPriv::Coro(ref c) => c.upgrade().map(Val::Coro),
            GcValPriv::RData(ref r) => r.upgrade().map(Val::RData),
            GcValPriv::RFn(ref r) => r.upgrade().map(Val::RFn),
        }
    }
}

//-------------------------------------------------------------------------------------------------
// Slot
//-------------------------------------------------------------------------------------------------

#[doc(hidden)]
#[derive(Clone)]
pub enum Slot {
    Nil,
    Int(i32),
    Flo(f32),
    Char(char),
    Bool(bool),
    Sym(Sym),
    Arr(Raw<Arr>),
    Str(Raw<Str>),
    Tab(Raw<Tab>),
    GIter(Raw<GIter>),
    Obj(Raw<Obj>),
    Class(Raw<Class>),
    GFn(Raw<GFn>),
    Coro(Raw<Coro>),
    RData(Raw<RData>),
    RFn(Raw<RFn>),
}

impl Slot {
    #[inline]
    pub(crate) fn from_val(val: &Val) -> Slot {
        match *val {
            Val::Nil => Slot::Nil,
            Val::Int(i) => Slot::Int(i),
            Val::Char(c) => Slot::Char(c),
            Val::Flo(f) => Slot::Flo(f),
            Val::Bool(b) => Slot::Bool(b),
            Val::Sym(s) => Slot::Sym(s),
            Val::Arr(ref a) => Slot::Arr(Raw::from_root(a)),
            Val::Str(ref s) => Slot::Str(Raw::from_root(s)),
            Val::Tab(ref t) => Slot::Tab(Raw::from_root(t)),
            Val::GIter(ref g) => Slot::GIter(Raw::from_root(g)),
            Val::Obj(ref o) => Slot::Obj(Raw::from_root(o)),
            Val::Class(ref c) => Slot::Class(Raw::from_root(c)),
            Val::GFn(ref g) => Slot::GFn(Raw::from_root(g)),
            Val::Coro(ref c) => Slot::Coro(Raw::from_root(c)),
            Val::RData(ref r) => Slot::RData(Raw::from_root(r)),
            Val::RFn(ref r) => Slot::RFn(Raw::from_root(r)),
        }
    }

    #[inline]
    pub(crate) fn root(&self) -> Val {
        match *self {
            Slot::Nil => Val::Nil,
            Slot::Int(i) => Val::Int(i),
            Slot::Char(c) => Val::Char(c),
            Slot::Flo(f) => Val::Flo(f),
            Slot::Bool(b) => Val::Bool(b),
            Slot::Sym(s) => Val::Sym(s),
            Slot::Arr(ref a) => Val::Arr(a.root()),
            Slot::Str(ref s) => Val::Str(s.root()),
            Slot::Tab(ref t) => Val::Tab(t.root()),
            Slot::GIter(ref g) => Val::GIter(g.root()),
            Slot::Obj(ref o) => Val::Obj(o.root()),
            Slot::Class(ref c) => Val::Class(c.root()),
            Slot::GFn(ref c) => Val::GFn(c.root()),
            Slot::Coro(ref c) => Val::Coro(c.root()),
            Slot::RData(ref r) => Val::RData(r.root()),
            Slot::RFn(ref r) => Val::RFn(r.root()),
        }
    }

    pub(crate) fn type_name(&self) -> &'static str {
        self.root().type_name()
    }

    pub(crate) fn a_type_name(&self) -> &'static str {
        self.root().a_type_name()
    }
}

//Slot implements Eq and Hash so that it can be used as HashMap key. unlike Val, its PartialEq
//implementation has the semantics of keys_eqv, rather than eq.
impl PartialEq<Slot> for Slot {
    #[inline]
    fn eq(&self, other: &Slot) -> bool {
        self.root().keys_eqv(&other.root())
    }
}

impl Eq for Slot {}

impl Hash for Slot {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Slot::Nil => Hashable(Val::Nil).hash(state),
            Slot::Int(i) => Hashable(Val::Int(i)).hash(state),
            Slot::Flo(f) => Hashable(Val::Flo(f)).hash(state),
            Slot::Char(c) => Hashable(Val::Char(c)).hash(state),
            Slot::Bool(b) => Hashable(Val::Bool(b)).hash(state),
            Slot::Sym(s) => Hashable(Val::Sym(s)).hash(state),
            Slot::Arr(ref raw) => (**raw).hash(state),
            Slot::Str(ref raw) => (**raw).hash(state),
            Slot::Tab(ref raw) => (&**raw as *const _ as usize).hash(state),
            Slot::GIter(ref raw) => (&**raw as *const _ as usize).hash(state),
            Slot::Obj(ref raw) => (&**raw as *const _ as usize).hash(state),
            Slot::Class(ref raw) => (&**raw as *const _ as usize).hash(state),
            Slot::GFn(ref raw) => (&**raw as *const _ as usize).hash(state),
            Slot::Coro(ref raw) => (&**raw as *const _ as usize).hash(state),
            Slot::RData(ref raw) => (&**raw as *const _ as usize).hash(state),
            Slot::RFn(ref raw) => (&**raw as *const _ as usize).hash(state),
        }
    }
}

//-------------------------------------------------------------------------------------------------
// Header
//-------------------------------------------------------------------------------------------------

/*
the gc-allocation header is made of two 32-bit words.

the high word dedicates its topmost eight bits to the Engine id, and the next bit to a "frozen"
flag. its lower 23 bits are an unsigned index into the root entries vec (or 0 when unrooted).
the root count is stored in the root entry, rather than inline, to avoid wasting space in the
header of unrooted objects.

this enforces quite a low limit on the number of rooted or weakly-rooted objects (8,388,607).
we might want to find a workaround for this - todo?

the lower word is 0x_ffff_ffff for a marked young object or 0x_ffff_fffe for an unmarked young
object. otherwise, the upper two bits are the color index, and the lower 30 bits are an unsigned
index into the old-objects vec for that color.

the frozen flag isn't actually written or read by the gc at all. we just store it in the gc
header because otherwise there would be several structs with a `frozen: Cell<bool>` field,
potentially taking up 64 bits of storage for 1 bit of information.
*/

const ENGINE_ID_SHIFT: u32 = 24;
const ENGINE_ID_MASK: u32 = 0xff << 24;
const FROZEN_BIT: u32 = 0x1 << 23;
const ROOT_INDEX_MASK: u32 = !(ENGINE_ID_MASK | FROZEN_BIT);
const MAX_ROOT_INDEX: u32 = ROOT_INDEX_MASK - 1;

const MARKED_YOUNG_BITS: u32 = 0x_ffff_ffff;
const UNMARKED_YOUNG_BITS: u32 = 0x_ffff_fffe;
const COLOR_SHIFT: u32 = 30;
const COLOR_MASK: u32 = 0x3 << 30;
const OLD_INDEX_MASK: u32 = !COLOR_MASK;
const MAX_OLD_INDEX: usize = (OLD_INDEX_MASK - 2) as usize;

#[doc(hidden)]
#[derive(Clone)]
pub struct Header {
    hi: Cell<u32>,
    lo: Cell<u32>,
}

impl Header {
    pub(crate) fn new() -> Header {
        let engine_id = with_heap(|heap| heap.engine_id as u32);
        Header {
            hi: Cell::new(engine_id << ENGINE_ID_SHIFT),
            lo: Cell::new(UNMARKED_YOUNG_BITS),
        }
    }

    fn reset(&self) {
        let engine_id = with_heap(|heap| heap.engine_id as u32);
        self.hi.set(engine_id << ENGINE_ID_SHIFT);
        self.lo.set(UNMARKED_YOUNG_BITS);
    }

    fn engine_id(&self) -> u8 {
        (self.hi.get() >> ENGINE_ID_SHIFT) as u8
    }

    #[inline]
    pub(crate) fn frozen(&self) -> bool {
        (self.hi.get() & FROZEN_BIT) != 0
    }

    #[inline]
    pub(crate) fn freeze(&self) {
        self.hi.set(self.hi.get() | FROZEN_BIT);
    }

    fn rooted(&self) -> bool {
        self.hi.get() & ROOT_INDEX_MASK != 0
    }

    fn root_index(&self) -> u32 {
        self.hi.get() & ROOT_INDEX_MASK
    }

    fn set_root_index(&self, root_index: u32) {
        debug_assert!(root_index <= MAX_ROOT_INDEX);
        self.hi.set((self.hi.get() & !ROOT_INDEX_MASK) | root_index);
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
        self.lo
            .set(((color_index as u32) << COLOR_SHIFT) | (old_index as u32));
    }

    fn color_index(&self) -> usize {
        debug_assert!(!self.young());
        (self.lo.get() >> COLOR_SHIFT) as usize
    }

    fn set_color_index(&self, color_index: usize) {
        debug_assert!(!self.young());
        debug_assert!(color_index <= 3);
        self.lo
            .set((self.lo.get() & !COLOR_MASK) | ((color_index as u32) << COLOR_SHIFT));
    }

    fn old_index(&self) -> usize {
        debug_assert!(!self.young());
        (self.lo.get() & OLD_INDEX_MASK) as usize
    }

    fn set_old_index(&self, old_index: usize) {
        debug_assert!(!self.young() && old_index <= MAX_OLD_INDEX);
        self.lo
            .set((self.lo.get() & !OLD_INDEX_MASK) | (old_index as u32));
    }
}

#[doc(hidden)]
pub trait Allocate: Sized + Erase {
    fn visit_raws<V: Visitor>(&self, visitor: &mut V);

    fn clear_raws(&self);

    //we have to make the Header an intrusive field on the allocated type, because
    //otherwise it would be impossible to write-barrier an Arr without holding a Raw<Arr>
    //or Root<Arr>
    fn header(&self) -> &Header;

    //this method does not consider Self, but does include any heap allocations which are
    //exclusively owned by Self (a Box or a Vec would count, but a  Root or an Rc wouldn't).
    //we've had to establish the memory usage of rust's std collections by peeking at their
    //source code; this information may become inaccurate as time passes.
    fn owned_memory_usage(&self) -> usize;

    fn memory_usage(&self) -> usize {
        size_of::<Self>() + self.owned_memory_usage()
    }
}

//-------------------------------------------------------------------------------------------------
// Visitor, GcVisitor
//-------------------------------------------------------------------------------------------------

#[doc(hidden)]
pub trait Visitor {
    fn visit_raw<T: Allocate>(&mut self, rc: &Raw<T>)
    where
        Self: Sized;

    fn visit_gc(&mut self, root_index: u32);

    fn visit_slot(&mut self, slot: &Slot)
    where
        Self: Sized,
    {
        match *slot {
            Slot::Nil
            | Slot::Int(_)
            | Slot::Char(_)
            | Slot::Flo(_)
            | Slot::Bool(_)
            | Slot::Sym(_) => (),
            Slot::Arr(ref a) => self.visit_raw(a),
            Slot::Str(ref s) => self.visit_raw(s),
            Slot::Tab(ref t) => self.visit_raw(t),
            Slot::GIter(ref g) => self.visit_raw(g),
            Slot::Obj(ref o) => self.visit_raw(o),
            Slot::Class(ref c) => self.visit_raw(c),
            Slot::GFn(ref g) => self.visit_raw(g),
            Slot::Coro(ref c) => self.visit_raw(c),
            Slot::RData(ref r) => self.visit_raw(r),
            Slot::RFn(ref r) => self.visit_raw(r),
        }
    }
}

struct MarkingVisitor<'a, 'b> {
    heap: &'a Heap,
    marking_stack: &'b mut RefMut<'a, Vec<ErasedRaw>>,
    old_objects: &'b mut [RefMut<'a, Vec<ErasedRaw>>; 4],
    old_only: bool,
}

impl<'a, 'b> MarkingVisitor<'a, 'b> {
    #[inline]
    fn new(
        heap: &'a Heap,
        marking_stack: &'b mut RefMut<'a, Vec<ErasedRaw>>,
        old_objects: &'b mut [RefMut<'a, Vec<ErasedRaw>>; 4],
        old_only: bool,
    ) -> MarkingVisitor<'a, 'b> {
        MarkingVisitor {
            heap,
            marking_stack,
            old_objects,
            old_only,
        }
    }
}

impl<'a, 'b> Visitor for MarkingVisitor<'a, 'b> {
    #[inline]
    fn visit_raw<T: Allocate>(&mut self, raw: &Raw<T>)
    where
        Self: Sized,
    {
        let header = raw.header();

        //if the target object is young and unmarked, mark it. if it's old and white, turn it gray.
        if header.young() {
            debug_assert!(!self.old_only);

            if !header.marked() {
                header.mark();
                self.marking_stack.push(T::erase_raw(raw.clone()));
            }
        } else {
            if header.color_index() == self.heap.white_index.get() {
                self.heap
                    .change_color(raw, self.heap.gray_index.get(), self.old_objects);
            }
        }
    }

    #[inline]
    fn visit_gc(&mut self, root_index: u32) {
        let root_storage = self.heap.root_storage.borrow();

        if let Some(erased) = root_storage.entries[root_index as usize].raw.as_ref() {
            with_erased_raw!(erased, raw, self.visit_raw(raw))
        }
    }
}

/**
Visitor passed to an `RData`'s [`trace` callback](struct.RClassBuilder.html#method.trace).
*/
pub struct GcVisitor<'a>(pub(crate) &'a mut dyn Visitor);

impl<'a> GcVisitor<'a> {
    #[inline]
    pub fn visit<T: Allocate>(&mut self, gc: &Gc<T>) {
        self.0.visit_gc(gc.root_index());
    }

    #[inline]
    pub fn visit_rgc<T>(&mut self, rgc: &RGc<T>) {
        self.0.visit_gc(rgc.0.root_index());
    }

    #[inline]
    pub fn visit_gc_val(&mut self, gc_val: &GcVal) {
        match gc_val.0 {
            GcValPriv::Nil => (),
            GcValPriv::Int(_) => (),
            GcValPriv::Char(_) => (),
            GcValPriv::Flo(_) => (),
            GcValPriv::Bool(_) => (),
            GcValPriv::Sym(_) => (),
            GcValPriv::Arr(ref a) => self.visit(a),
            GcValPriv::Str(ref s) => self.visit(s),
            GcValPriv::Tab(ref t) => self.visit(t),
            GcValPriv::GIter(ref g) => self.visit(g),
            GcValPriv::Obj(ref o) => self.visit(o),
            GcValPriv::Class(ref c) => self.visit(c),
            GcValPriv::GFn(ref g) => self.visit(g),
            GcValPriv::Coro(ref c) => self.visit(c),
            GcValPriv::RData(ref r) => self.visit(r),
            GcValPriv::RFn(ref r) => self.visit(r),
        }
    }
}

//-------------------------------------------------------------------------------------------------
// Heap
//-------------------------------------------------------------------------------------------------

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
    pub(crate) engine_id: u8,
    pub(crate) recycler: Recycler,
    gc_in_progress: Cell<bool>,

    young_objects: RefCell<Vec<ErasedRaw>>,
    young_bytes: Cell<usize>,
    marking_stack: RefCell<Vec<ErasedRaw>>,

    old_objects: [RefCell<Vec<ErasedRaw>>; 4],
    old_bytes: [Cell<usize>; 4],
    white_index: Cell<usize>,
    gray_index: Cell<usize>,
    black_index: Cell<usize>,
    ghost_index: Cell<usize>,

    pub(crate) root_storage: RefCell<RootStorage>,

    black_target: Cell<usize>,
    ghost_target: Cell<usize>,

    ratio_u: Cell<f32>,
    ratio_r: Cell<f32>,
    ratio_w: Cell<Option<f32>>,
}

impl Drop for Heap {
    fn drop(&mut self) {
        let root_storage = self.root_storage.borrow();
        for entry in &root_storage.entries {
            if entry.strong_count > 0 || entry.weak_count > 0 {
                eprintln!("a Root or Gc has outlived its originating Runtime - aborting process");
                abort()
            }
        }
    }
}

impl Heap {
    pub(crate) fn new(engine_id: u8) -> Heap {
        Heap {
            engine_id,
            recycler: Recycler::new(),
            gc_in_progress: Cell::new(false),

            young_objects: RefCell::new(Vec::new()),
            young_bytes: Cell::new(0),
            marking_stack: RefCell::new(Vec::new()),

            old_objects: [
                RefCell::new(Vec::new()),
                RefCell::new(Vec::new()),
                RefCell::new(Vec::new()),
                RefCell::new(Vec::new()),
            ],
            old_bytes: [Cell::new(0), Cell::new(0), Cell::new(0), Cell::new(0)],
            white_index: Cell::new(0),
            gray_index: Cell::new(1),
            black_index: Cell::new(2),
            ghost_index: Cell::new(3),

            root_storage: RefCell::new(RootStorage::new()),

            black_target: Cell::new(0),
            ghost_target: Cell::new(0),

            ratio_u: Cell::new(INITIAL_U),
            ratio_r: Cell::new(INITIAL_R),
            ratio_w: Cell::new(INITIAL_W),
        }
    }

    pub(crate) fn clear(&self) {
        for erased in self.young_objects.borrow_mut().drain(..) {
            with_erased_raw!(erased, raw, raw.free())
        }

        self.young_bytes.set(0);
        self.marking_stack.borrow_mut().clear();

        for i in 0..4 {
            for erased in self.old_objects[i].borrow_mut().drain(..) {
                with_erased_raw!(erased, raw, raw.free())
            }

            self.old_bytes[i].set(0);
        }

        self.black_target.set(0);
        self.ghost_target.set(0);

        self.ratio_u.set(INITIAL_U);
        self.ratio_r.set(INITIAL_R);
        self.ratio_w.set(INITIAL_W);

        //we don't clear self.root_storage, because we need it to check for extant
        //Roots when the Heap is dropped.
    }

    pub(crate) fn all_unfreed_rdata(&self) -> Vec<Root<RData>> {
        let object_borrows = [
            self.young_objects.borrow(),
            self.old_objects[0].borrow(),
            self.old_objects[1].borrow(),
            self.old_objects[2].borrow(),
            self.old_objects[3].borrow(),
        ];

        let mut rdata = Vec::new();

        for erased_raw_vec in &object_borrows {
            for erased_raw in &**erased_raw_vec {
                if let ErasedRaw::RData(raw) = erased_raw {
                    let root = Root::new(Raw::clone(raw));

                    if !root.is_freed() {
                        rdata.push(root);
                    }
                }
            }
        }

        rdata
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
        Root::new(self.alloc_raw(init))
    }

    pub(crate) fn alloc_raw<T: Allocate>(&self, init: T) -> Raw<T> {
        assert!(
            !self.gc_in_progress.get(),
            "attempted to allocate during a gc step"
        );

        let raw = Raw::new(init);
        self.register_young(raw.clone());
        raw
    }

    fn register_young<T: Allocate>(&self, raw: Raw<T>) {
        let header = raw.header();
        debug_assert!(header.young() && !header.marked());

        self.young_bytes
            .set(self.young_bytes.get() + raw.memory_usage());
        self.young_objects.borrow_mut().push(T::erase_raw(raw));
    }

    fn promote<T: Allocate>(
        &self,
        raw: &Raw<T>,
        old_objects: &mut [RefMut<Vec<ErasedRaw>>; 4],
    ) -> usize {
        debug_assert!(raw.header().young());

        let black_index = self.black_index.get();

        old_objects[black_index].push(T::erase_raw(raw.clone()));
        raw.header()
            .promote(black_index, old_objects[black_index].len() - 1);

        let memory_usage = raw.memory_usage();
        self.old_bytes[black_index].set(self.old_bytes[black_index].get() + memory_usage);

        memory_usage
    }

    fn change_color<T: Allocate>(
        &self,
        raw: &Raw<T>,
        new_color_index: usize,
        old_objects: &mut [RefMut<Vec<ErasedRaw>>; 4],
    ) {
        let header = raw.header();
        debug_assert!(!header.young());

        let prev_color_index = header.color_index();
        debug_assert!(prev_color_index != new_color_index);

        //remove from the previous old_objects vec, using swap_remove. update the swapped object's
        //old_index to reflect its new position. this is similar to how we handle unrooting.
        let prev_i = header.old_index();
        let erased = old_objects[prev_color_index].swap_remove(prev_i);
        if prev_i < old_objects[prev_color_index].len() {
            old_objects[prev_color_index][prev_i]
                .header()
                .set_old_index(prev_i);
        }

        //push onto the destination old_objects vec and update our old_index
        old_objects[new_color_index].push(erased);
        header.set_old_index(old_objects[new_color_index].len() - 1);

        //update the object's color bits
        header.set_color_index(new_color_index);

        //update old_bytes
        let usage = raw.memory_usage();
        self.old_bytes[prev_color_index].set(self.old_bytes[prev_color_index].get() - usage);
        self.old_bytes[new_color_index].set(self.old_bytes[new_color_index].get() + usage);
    }

    //the caller is required to to write-barrier anything that's in the grey memory-areas (those
    //which aren't write-barriered when mutated) just before calling collect_*.
    pub(crate) fn step(&self) {
        self.gc_in_progress.set(true);
        let _in_progress_guard = Guard::new(|| self.gc_in_progress.set(false));

        let mut young_objects = self.young_objects.borrow_mut();
        let mut old_objects = [
            self.old_objects[0].borrow_mut(),
            self.old_objects[1].borrow_mut(),
            self.old_objects[2].borrow_mut(),
            self.old_objects[3].borrow_mut(),
        ];
        let mut marking_stack = self.marking_stack.borrow_mut();

        let white_index = self.white_index.get();
        let gray_index = self.gray_index.get();
        let black_index = self.black_index.get();
        let ghost_index = self.ghost_index.get();

        //traverse all of the strong roots
        let root_storage = self.root_storage.borrow();
        let mut strong_entry_i = root_storage.first_strong;
        while strong_entry_i != NULL_ENTRY {
            let entry = &root_storage.entries[strong_entry_i as usize];

            let erased_raw = entry.raw.as_ref().unwrap();
            with_erased_raw!(erased_raw, raw, {
                let mut visitor =
                    MarkingVisitor::new(self, &mut marking_stack, &mut old_objects, false);
                visitor.visit_raw(raw);
            });

            strong_entry_i = entry.next_entry;
        }
        drop(root_storage);

        //mark young objects: until the marking stack is empty, pop an object off it, mark
        //all of its young pointees and add them to the stack, and mark all of its old white
        //pointees as gray.
        while let Some(erased) = marking_stack.pop() {
            with_erased_raw!(erased, raw, {
                let mut visitor =
                    MarkingVisitor::new(self, &mut marking_stack, &mut old_objects, false);
                raw.visit_raws(&mut visitor);
            })
        }

        //sweep young objects: empty the young generation. if a young object is unmarked, free it
        //immediately. if it's marked, promote it into an old black object.
        let mut promoted_bytes: usize = 0;

        for erased in young_objects.drain(..) {
            with_erased_raw!(erased, raw, {
                let header = raw.header();
                if header.marked() {
                    promoted_bytes += self.promote(raw, &mut old_objects);
                } else {
                    if header.rooted() {
                        let mut root_storage = self.root_storage.borrow_mut();
                        let root_entry = &mut root_storage.entries[header.root_index() as usize];

                        debug_assert!(root_entry.strong_count == 0);
                        debug_assert!(root_entry.raw.is_some());
                        root_entry.raw = None;

                        header.set_root_index(0);
                    }

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

        while self.old_bytes[black_index].get() < self.black_target.get()
            && !old_objects[gray_index].is_empty()
        {
            let erased = old_objects[gray_index].last().unwrap().clone();

            with_erased_raw!(erased, raw, {
                self.change_color(raw, black_index, &mut old_objects);

                let mut visitor =
                    MarkingVisitor::new(self, &mut marking_stack, &mut old_objects, true);
                raw.visit_raws(&mut visitor);
            })
        }

        //if there were a nonzero number of ghost objects last cycle, ratio_w will be Some.
        //free (promoted_bytes * W/U) bytes of ghost objects.
        if let Some(ratio_w) = self.ratio_w.get() {
            let bytes_to_free = (ratio_w * target_incr as f32).ceil() as usize;
            self.ghost_target
                .set(self.ghost_target.get().saturating_sub(bytes_to_free));

            while self.old_bytes[ghost_index].get() > self.ghost_target.get() {
                let erased = old_objects[ghost_index].pop().unwrap();

                //note that with "unsafe-internals" disabled, this may cause latency spikes by
                //suddenly freeing a tree of Rc references all at once. we could solve this by
                //splitting it into two incremental passes: clear_raws() followed by deleting the
                //ghost ErasedRaw itself.
                with_erased_raw!(erased, raw, {
                    let memory_usage = raw.memory_usage();
                    self.old_bytes[ghost_index]
                        .set(self.old_bytes[ghost_index].get() - memory_usage);

                    let header = raw.header();
                    if header.rooted() {
                        let mut root_storage = self.root_storage.borrow_mut();
                        let root_entry = &mut root_storage.entries[header.root_index() as usize];

                        debug_assert!(root_entry.strong_count == 0);
                        debug_assert!(root_entry.raw.is_some());
                        root_entry.raw = None;

                        header.set_root_index(0);
                    }
                });

                self.recycler.free(erased);
            }
        } else {
            debug_assert!(old_objects[ghost_index].is_empty());
        }

        //if there are no gray objects left, and if we've produced at least MIN_SURVIVING_BYTES
        //of old black objects, then we've reached the end of the cycle. make all white objects
        //into ghost objects, update W, and turn all black objects white.
        if old_objects[gray_index].is_empty()
            && self.old_bytes[black_index].get() >= MIN_SURVIVING_BYTES
        {
            //if there are any remaining ghost objects (unlikely unless the surviving heap has
            //sharply decreased in size), we need to change each object's color index so that
            //they'll remain ghosts rather than turning black.
            while let Some(erased) = old_objects[ghost_index].pop() {
                old_objects[white_index].push(erased.clone());

                let header = erased.header();
                header.set_color_index(white_index);
                header.set_old_index(old_objects[white_index].len() - 1);
            }

            self.old_bytes[white_index]
                .set(self.old_bytes[white_index].get() + self.old_bytes[ghost_index].get());
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
            self.ghost_target
                .set(self.old_bytes[self.ghost_index.get()].get());
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
        self.old_bytes[self.white_index.get()].get()
            + self.old_bytes[self.gray_index.get()].get()
            + self.old_bytes[self.black_index.get()].get()
    }

    pub(crate) fn ghost_memory_usage(&self) -> usize {
        self.old_bytes[self.ghost_index.get()].get()
    }

    pub(crate) fn traverse_stack_slot(&self, dst: &Slot) {
        match *dst {
            Slot::Nil
            | Slot::Int(_)
            | Slot::Char(_)
            | Slot::Flo(_)
            | Slot::Bool(_)
            | Slot::Sym(_) => (),
            Slot::Arr(ref raw) => self.traverse_stack_raw(raw),
            Slot::Str(ref raw) => self.traverse_stack_raw(raw),
            Slot::Tab(ref raw) => self.traverse_stack_raw(raw),
            Slot::GIter(ref raw) => self.traverse_stack_raw(raw),
            Slot::Obj(ref raw) => self.traverse_stack_raw(raw),
            Slot::Class(ref raw) => self.traverse_stack_raw(raw),
            Slot::GFn(ref raw) => self.traverse_stack_raw(raw),
            Slot::Coro(ref raw) => self.traverse_stack_raw(raw),
            Slot::RData(ref raw) => self.traverse_stack_raw(raw),
            Slot::RFn(ref raw) => self.traverse_stack_raw(raw),
        }
    }

    pub(crate) fn traverse_stack_raw<T: Allocate>(&self, dst: &Raw<T>) {
        let header = dst.header();

        //this should be impossible, but we check it for an extra level of security anyway
        if self.engine_id != header.engine_id() {
            eprintln!("attempted to move a Root to another Runtime - aborting process");
            abort()
        }

        //if dst is young, mark it. if it's old and white, turn it gray. this is copied from
        //write_barrier(), below.
        if header.young() {
            if !header.marked() {
                self.marking_stack
                    .borrow_mut()
                    .push(T::erase_raw(dst.clone()));
                header.mark();
            }
        } else {
            if header.color_index() == self.white_index.get() {
                let mut old_objects = [
                    self.old_objects[0].borrow_mut(),
                    self.old_objects[1].borrow_mut(),
                    self.old_objects[2].borrow_mut(),
                    self.old_objects[3].borrow_mut(),
                ];

                self.change_color(dst, self.gray_index.get(), &mut old_objects);
            }
        }
    }

    pub(crate) fn write_barrier_val<T: Allocate>(&self, src: &T, dst: &Val) {
        match *dst {
            Val::Nil | Val::Int(_) | Val::Char(_) | Val::Flo(_) | Val::Bool(_) | Val::Sym(_) => (),
            Val::Arr(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::Str(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::Tab(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::GIter(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::Obj(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::Class(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::GFn(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::Coro(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::RData(ref root) => self.write_barrier(src, &root.to_raw()),
            Val::RFn(ref root) => self.write_barrier(src, &root.to_raw()),
        }
    }

    pub(crate) fn write_barrier_slot<T: Allocate>(&self, src: &T, dst: &Slot) {
        match *dst {
            Slot::Nil
            | Slot::Int(_)
            | Slot::Char(_)
            | Slot::Flo(_)
            | Slot::Bool(_)
            | Slot::Sym(_) => (),
            Slot::Arr(ref raw) => self.write_barrier(src, raw),
            Slot::Str(ref raw) => self.write_barrier(src, raw),
            Slot::Tab(ref raw) => self.write_barrier(src, raw),
            Slot::GIter(ref raw) => self.write_barrier(src, raw),
            Slot::Obj(ref raw) => self.write_barrier(src, raw),
            Slot::Class(ref raw) => self.write_barrier(src, raw),
            Slot::GFn(ref raw) => self.write_barrier(src, raw),
            Slot::Coro(ref raw) => self.write_barrier(src, raw),
            Slot::RData(ref raw) => self.write_barrier(src, raw),
            Slot::RFn(ref raw) => self.write_barrier(src, raw),
        }
    }

    #[inline]
    pub(crate) fn write_barrier<T, U>(&self, src: &T, dst: &Raw<U>)
    where
        T: Allocate,
        U: Allocate,
    {
        /*
        we're limited here by the fact that src has to be &T rather than &Raw<T>. ideally we
        would track old cross-references into the young generation via a pointer to src rather
        than dst, so that we can re-traverse src at the start of the young collection, so that
        young objects aren't being promoted to the old generation just because an old object
        happened to point to them at least once during the previous frame.

        i anticipate this should be fairly rare (most short-lived objects will only be pointed
        to by stack variables or by other short-lived objects), so we can live with it for now.
        */

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
                    self.marking_stack
                        .borrow_mut()
                        .push(U::erase_raw(dst.clone()));
                    dst_header.mark();
                }
            } else {
                if dst_header.color_index() == self.white_index.get()
                    && src_header.color_index() == self.black_index.get()
                {
                    let mut old_objects = [
                        self.old_objects[0].borrow_mut(),
                        self.old_objects[1].borrow_mut(),
                        self.old_objects[2].borrow_mut(),
                        self.old_objects[3].borrow_mut(),
                    ];

                    self.change_color(dst, self.gray_index.get(), &mut old_objects);
                }
            }
        }
    }

    #[inline]
    pub(crate) fn write_barrier_rdata(&self, rdata: &Root<RData>) {
        /*
        we could conceivably traverse all of the RData's gcs when write_barrier is invoked.
        however, this would be complex (we'd need to define a new Visitor type), and it
        wouldn't improve any of our safety or correctness guarantees (because we can never
        rely on the user calling glsp::write_barrier correctly).

        instead, we "turn back the clock": if an old black RData is mutated, we turn it gray,
        so that it must be traversed again before any of its pointees can be deallocated for
        being unreachable. this makes glsp::write_barrier easier to use correctly... it will
        work fine, as long as it's called at least once between the RData's mutation and the
        next call to (gc).
        */

        if !rdata.header().young() {
            if rdata.header().color_index() == self.black_index.get() {
                let mut old_objects = [
                    self.old_objects[0].borrow_mut(),
                    self.old_objects[1].borrow_mut(),
                    self.old_objects[2].borrow_mut(),
                    self.old_objects[3].borrow_mut(),
                ];

                self.change_color(rdata.as_raw(), self.gray_index.get(), &mut old_objects);
            }
        }
    }

    #[inline]
    pub(crate) fn memory_usage_barrier<T>(&self, src: &T, prev_usage: usize, cur_usage: usize)
    where
        T: Allocate,
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
    arrs: Vec<RefCell<Vec<Raw<Arr>>>>,
    giters: RefCell<Vec<Raw<GIter>>>,
}

impl Recycler {
    fn new() -> Recycler {
        Recycler {
            arrs: vec![RefCell::new(Vec::new()); 32],
            giters: RefCell::new(Vec::new()),
        }
    }

    fn free(&self, erased: ErasedRaw) {
        match erased {
            ErasedRaw::Arr(arr) => {
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
            ErasedRaw::GIter(giter) => {
                let header = giter.header();
                debug_assert!(!header.rooted());
                header.reset();

                giter.clear_raws();
                self.giters.borrow_mut().push(giter);
            }
            ErasedRaw::Str(raw) => raw.free(),
            ErasedRaw::Tab(raw) => raw.free(),
            ErasedRaw::Obj(raw) => raw.free(),
            ErasedRaw::Class(raw) => raw.free(),
            ErasedRaw::GFn(raw) => raw.free(),
            ErasedRaw::Stay(raw) => raw.free(),
            ErasedRaw::Coro(raw) => raw.free(),
            ErasedRaw::RData(raw) => raw.free(),
            ErasedRaw::RFn(raw) => raw.free(),
            ErasedRaw::Bytecode(raw) => raw.free(),
            ErasedRaw::Lambda(raw) => raw.free(),
        }
    }

    pub(crate) fn arr(&self) -> Root<Arr> {
        for arrs in &self.arrs {
            let mut arrs = arrs.borrow_mut();
            if !arrs.is_empty() {
                let arr = arrs.pop().unwrap().into_root();
                drop(arrs);

                with_heap(|heap| heap.register_young(arr.to_raw()));
                return arr;
            }
        }

        glsp::alloc(Arr::new())
    }

    pub(crate) fn arr_with_capacity(&self, capacity: usize) -> Root<Arr> {
        for i in capacity..MAX_ARR_CAPACITY {
            let mut arrs = self.arrs[i].borrow_mut();
            if !arrs.is_empty() {
                let arr = arrs.pop().unwrap().into_root();
                drop(arrs);

                with_heap(|heap| heap.register_young(arr.to_raw()));
                return arr;
            }
        }

        glsp::alloc(Arr::with_capacity(capacity))
    }

    pub(crate) fn arr_from_elem<V>(&self, elem: V, reps: usize) -> GResult<Root<Arr>>
    where
        V: Clone + IntoVal,
    {
        for i in reps..MAX_ARR_CAPACITY {
            let mut arrs = self.arrs[i].borrow_mut();
            if !arrs.is_empty() {
                let arr = arrs.pop().unwrap().into_root();
                drop(arrs);

                with_heap(|heap| heap.register_young(arr.to_raw()));

                for _ in 0..reps.saturating_sub(1) {
                    arr.push(elem.clone())?;
                }

                if reps >= 1 {
                    arr.push(elem)?;
                }

                return Ok(arr);
            }
        }

        Ok(glsp::alloc(Arr::from_elem(elem, reps)?))
    }

    pub(crate) fn arr_from_iter<T>(&self, source: T) -> GResult<Root<Arr>>
    where
        T: IntoIterator,
        T::Item: IntoVal,
    {
        let iter = source.into_iter();
        let (min_size, max_size) = iter.size_hint();

        if max_size.is_some() && min_size == max_size.unwrap() {
            let capacity = min_size;
            for i in capacity..MAX_ARR_CAPACITY {
                let mut arrs = self.arrs[i].borrow_mut();
                if !arrs.is_empty() {
                    let arr = arrs.pop().unwrap().into_root();
                    drop(arrs);

                    with_heap(|heap| heap.register_young(arr.to_raw()));

                    for item in iter {
                        arr.push(item)?;
                    }

                    return Ok(arr);
                }
            }
        }

        Ok(glsp::alloc(Arr::from_iter(iter)?))
    }

    pub(crate) fn giter(&self, state: GIterState) -> Root<GIter> {
        if let Some(raw) = self.giters.borrow_mut().pop() {
            let giter = raw.root();
            with_heap(|heap| heap.register_young(raw));

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
