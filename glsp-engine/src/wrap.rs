use smallvec::{SmallVec};
use std::{i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize, str};
use std::any::{type_name};
use std::cell::{Ref};
use std::cmp::{Ordering};
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::convert::{TryInto};
use std::hash::{BuildHasher, Hash};
use std::io::{Write};
use std::iter::{Extend, FromIterator};
use std::ffi::{CString, CStr, OsString, OsStr};
use std::path::{Path, PathBuf};
use super::code::{Coro, GFn};
use super::collections::{Arr, DequeAccess, DequeOps, Deque, Str, Tab};
use super::class::{Class, Obj};
use super::engine::{glsp, RData, RFn, RRoot, RStore, stock_syms::*, Sym};
use super::error::{GResult};
use super::eval::{EnvMode, Expander};
use super::gc::{Gc, Root, Slot};
use super::iter::{GIter, Iterable, GIterLen};
use super::val::{Num, Val};

/*
rfn!() takes an arbitrary function or non-capturing closure whose arguments all implement
MakeArg and whose return value implements IntoResult. it returns a minimum and maximum arg
count, and a monomorphized fn ptr, `fn(&[Slot]) -> GResult<Slot>`

MakeArg is implemented for a long list of types: anything that implements FromVal, and also 
references to libraries, references to RData, references to string types and primitive types
like Arr, Option<T> for optional args, &[T] for rest arguments, and OrNil<T>.

could potentially support Option<T>, &[T] and &mut [T] in tuples?

if we find a way for pattern-matches to "cheaply fail" (perhaps returning a new result type
which encodes the reason for the conversion failure as plain old data, rather than stringifying
it straight away?), then we could support the `either` crate. this would make OrNil
unnecessary; replace it with Either<(), T>.
*/

/*
notes on the implementation:

the ToVal and FromVal traits are user-facing. if the user has or wants a Val, they can invoke 
these traits directly with say i32::from_val(val) or my_i32.to_val(). the user can implement 
these traits for their own types.

both of those traits have a "secret" method: into_slot or from_slot respectively. they are 
doc(hidden), with a default implementation that works for any valid to_val or from_val 
implementation. the "secret" methods are invoked whenever rust data is passed into or out of 
glsp's internals (rfn return values, and the arguments and return values to various methods 
on Arr, Tab, Obj, Class and glsp::). we implement the "secret" methods for types which want 
to avoid creating a temporary root, like Val or Root<Arr>.

we want arguments to methods like tab.get(key) to perform autoderef: that is, they should 
accept i32, &i32, &&&i32, &mut i32, etc. we achieve that by blanket-implementing ToVal
for T where T: Deref, T::Target: ToVal. note that this prevents the user from directly 
implementing ToVal for anything that implements Deref.

the MakeArg trait, and its parent MakeTemp, are used for rfn arguments. they have a blanket
implementation over T: FromVal. they also provide implementations for certain "special"
types like Option<T>, &Arr, and &[T]. they're implemented individually for references
to any T that's been passed to the lib! {} or rdata! {} macros.

the IntoResult trait is used for rfn results. it has a blanket implementation over T: ToVal,
and it's also implemented for GResult<T>.
*/


//-------------------------------------------------------------------------------------------------
// ToVal, FromVal
//-------------------------------------------------------------------------------------------------

/**
A type which can be converted to a GameLisp value.

Many functions in the `glsp` crate receive a generic parameter `T: ToVal`. This enables those 
functions to accept many different Rust types, which will be silently converted to a 
[`Val`](enum.Val.html).
	
	glsp::set_global("numbers", (0, 1, 2, 3, 4))?;
	arr.push("text")?;

Implementing the `ToVal` trait for your own types will enable them to take advantage of automatic
conversions for `RFn` return values.
	
	struct Rgb(u8, u8, u8);

	impl ToVal for Rgb {
		fn to_val(&self) -> GResult<Val> {
			let Rgb(r, g, b) = self;
			arr![r, g, b].to_val()
		}
	}

	fn light_sea_green() -> Rgb {
		Rgb(32, 178, 170)
	}

	glsp::bind_rfn("light-sea-green", rfn!(light_sea_green))?;

Invoking a type's [`to_val` method](#method.to_val) is usually the most convenient way to 
produce a `Val`. `ToVal` is part of the [prelude](prelude/index.html), so there's no need to 
import it into scope.
	
	let thousand = 10.0_f64.pow(3.0).to_val()?;
*/

//we go for by-reference &self, &Val and &Slot, mostly so that we can blanket-implement
//ToVal for T where T: Deref, T::Target: ToVal, and so that we don't have to copy Slots
//out of the &[Slot] argument slice when constructing rfn parameters, or e.g. out of the
//VecDeque<Slot> when accessing an Arr.

pub trait ToVal {
	fn to_val(&self) -> GResult<Val>;

	#[doc(hidden)]
	fn to_slot(&self) -> GResult<Slot> {
		self.to_val()?.to_slot()
	}
}

/**
A type which can be converted from a GameLisp value.

Many functions in the `glsp` crate have a generic return value `R: FromVal`. They can 
automatically convert their return value to many different Rust types.
	
	let numbers: Vec<u8> = glsp::global("numbers")?;
	let text: Root<Str> = arr.pop()?;

Implementing the `FromVal` trait for your own types will also enable them to take advantage of 
automatic conversions for `RFn` arguments.
	
	struct Rgb(u8, u8, u8);

	impl FromVal for Rgb {
		fn from_val(val: &Val) -> GResult<Rgb> {
			let (r, g, b) = <(u8, u8, u8)>::from_val(val)?; 
			Ok(Rgb(r, g, b))
		}
	}

	fn invert(src: Rgb) -> Rgb {
		let Rgb(r, g, b) = src;
		Rgb(255 - r, 255 - g, 255 - b)
	}

	glsp::bind_rfn("invert", rfn!(invert))?;

Writing `T::from_val(v)?` is usually the most convenient way to destructure a `Val`. `FromVal`
is part of the [prelude](prelude/index.html), so there's no need to import it into scope.
	
	let f = f64::from_val(val)?;
*/

pub trait FromVal: Sized {
	//todo: should this be from_val<V: Borrow<Val>>? would be slightly more convenient when the
	//user is invoking it directly: they could pass either a Val or a &Val.
	fn from_val(val: &Val) -> GResult<Self>;

	#[doc(hidden)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		Self::from_val(&val.root())
	}
}

impl<'a, T> ToVal for &'a T where T: ToVal + ?Sized {
	fn to_val(&self) -> GResult<Val> {
		(**self).to_val()
	}

	fn to_slot(&self) -> GResult<Slot> {
		(**self).to_slot()
	}
}

impl<'a, T> ToVal for &'a mut T where T: ToVal + ?Sized {
	fn to_val(&self) -> GResult<Val> {
		(**self).to_val()
	}

	fn to_slot(&self) -> GResult<Slot> {
		(**self).to_slot()
	}
}


//-------------------------------------------------------------------------------------------------
// ToVal implementations
//-------------------------------------------------------------------------------------------------

impl ToVal for Val {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		Ok((*self).clone())
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		Ok(Slot::from_val(self))
	}
}

impl ToVal for Slot {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		Ok((*self).root())
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		Ok((*self).clone())
	}
}

impl<T> ToVal for Option<T> where T: ToVal {
	fn to_val(&self) -> GResult<Val> {
		match self {
			Some(src) => src.to_val(),
			None => Ok(Val::Nil)
		}
	}

	fn to_slot(&self) -> GResult<Slot> {
		match self {
			Some(src) => src.to_slot(),
			None => Ok(Slot::Nil)
		}
	}
}

impl ToVal for () {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		Ok(Val::Nil)
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		Ok(Slot::Nil)
	}
}

macro_rules! impl_to_val_infallible {
	($self_type:ty, $variant:ident) => (
		impl ToVal for $self_type {
			#[inline(always)]
			fn to_val(&self) -> GResult<Val> {
				Ok(Val::$variant((*self).into()))
			}

			#[inline(always)]
			fn to_slot(&self) -> GResult<Slot> {
				Ok(Slot::$variant((*self).into()))
			}
		}
	);
}

impl_to_val_infallible!(i8, Int);
impl_to_val_infallible!(i16, Int);
impl_to_val_infallible!(i32, Int);
impl_to_val_infallible!(u8, Int);
impl_to_val_infallible!(u16, Int);
impl_to_val_infallible!(f32, Flo);
impl_to_val_infallible!(char, Char);
impl_to_val_infallible!(bool, Bool);
impl_to_val_infallible!(Sym, Sym);
impl_to_val_infallible!(RFn, RFn);

macro_rules! impl_to_val_root {
	($t:ty, $variant:ident) => (
		impl ToVal for Root<$t> {
			#[inline(always)]
			fn to_val(&self) -> GResult<Val> {
				Ok(Val::$variant(self.clone()))
			}

			#[inline(always)]
			fn to_slot(&self) -> GResult<Slot> {
				Ok(Slot::$variant(Gc::from_root(self)))
			}
		}

		impl ToVal for Gc<$t> {
			#[inline(always)]
			fn to_val(&self) -> GResult<Val> {
				Ok(Val::$variant(self.root()))
			}

			#[inline(always)]
			fn to_slot(&self) -> GResult<Slot> {
				Ok(Slot::$variant(self.clone()))
			}
		}
	);
}

impl_to_val_root!(Arr, Arr);
impl_to_val_root!(Str, Str);
impl_to_val_root!(Tab, Tab);
impl_to_val_root!(GIter, GIter);
impl_to_val_root!(Obj, Obj);
impl_to_val_root!(Class, Class);
impl_to_val_root!(GFn, GFn);
impl_to_val_root!(Coro, Coro);
impl_to_val_root!(RData, RData);

impl<T: RStore> ToVal for RRoot<T> {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		Ok(Val::RData(self.to_root()))
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		Ok(Slot::RData(self.to_gc()))
	}
}

impl ToVal for Deque {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		match *self {
			Deque::Arr(ref root) => Ok(Val::Arr(root.clone())),
			Deque::Str(ref root) => Ok(Val::Str(root.clone()))
		}
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		match *self {
			Deque::Arr(ref root) => Ok(Slot::Arr(root.to_gc())),
			Deque::Str(ref root) => Ok(Slot::Str(root.to_gc()))
		}
	}
}

impl ToVal for Callable {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		match *self {
			Callable::GFn(ref root) => Ok(Val::GFn(root.clone())),
			Callable::RFn(rfn) => Ok(Val::RFn(rfn)),
			Callable::Class(ref root) => Ok(Val::Class(root.clone()))
		}
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		match *self {
			Callable::GFn(ref root) => Ok(Slot::GFn(Gc::from_root(root))),
			Callable::RFn(rfn) => Ok(Slot::RFn(rfn)),
			Callable::Class(ref root) => Ok(Slot::Class(Gc::from_root(root)))
		}
	}
}

impl ToVal for Expander {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		match *self {
			Expander::GFn(ref root) => Ok(Val::GFn(root.clone())),
			Expander::RFn(rfn) => Ok(Val::RFn(rfn))
		}
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		match *self {
			Expander::GFn(ref root) => Ok(Slot::GFn(Gc::from_root(root))),
			Expander::RFn(rfn) => Ok(Slot::RFn(rfn))
		}
	}
}

impl ToVal for Iterable {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		match self {
			Iterable::Arr(root) => Ok(Val::Arr(root.clone())),
			Iterable::Str(root) => Ok(Val::Str(root.clone())),
			Iterable::Tab(root) => Ok(Val::Tab(root.clone())),
			Iterable::GIter(root) => Ok(Val::GIter(root.clone())),
			Iterable::Coro(root) => Ok(Val::Coro(root.clone()))
		}
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		match self {
			Iterable::Arr(root) => Ok(Slot::Arr(Gc::from_root(root))),
			Iterable::Str(root) => Ok(Slot::Str(Gc::from_root(root))),
			Iterable::Tab(root) => Ok(Slot::Tab(Gc::from_root(root))),
			Iterable::GIter(root) => Ok(Slot::GIter(Gc::from_root(root))),
			Iterable::Coro(root) => Ok(Slot::Coro(Gc::from_root(root)))
		}
	}
}

impl ToVal for GIterLen {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		match *self {
			GIterLen::Exact(len) => Ok(Val::Int(len as i32)),
			GIterLen::Infinite => Ok(Val::Sym(INFINITE_SYM)),
			GIterLen::Unknown => Ok(Val::Sym(UNKNOWN_SYM))
		}
	}
}

impl ToVal for Ordering {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		match *self {
			Ordering::Less => Ok(Val::Sym(LT_SYM)),
			Ordering::Equal => Ok(Val::Sym(NUM_EQ_SYM)),
			Ordering::Greater => Ok(Val::Sym(GT_SYM))
		}
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		match *self {
			Ordering::Less => Ok(Slot::Sym(LT_SYM)),
			Ordering::Equal => Ok(Slot::Sym(NUM_EQ_SYM)),
			Ordering::Greater => Ok(Slot::Sym(GT_SYM))
		}
	}
}

macro_rules! impl_to_val_bounded_int {
	($self_type:ty) => (
		impl ToVal for $self_type {
			#[inline(always)]
			fn to_val(&self) -> GResult<Val> {
				if let Ok(converted) = (*self).try_into() {
					Ok(Val::Int(converted))
				} else {
					bail!("the result was {}, which is outside the range of an i32", self)
				}
			}

			#[inline(always)]
			fn to_slot(&self) -> GResult<Slot> {
				if let Ok(converted) = (*self).try_into() {
					Ok(Slot::Int(converted))
				} else {
					bail!("the result was {}, which is outside the range of an i32", self)
				}
			}
		}
	);
}

impl_to_val_bounded_int!(i64);
impl_to_val_bounded_int!(i128);
impl_to_val_bounded_int!(isize);
impl_to_val_bounded_int!(u32);
impl_to_val_bounded_int!(u64);
impl_to_val_bounded_int!(u128);
impl_to_val_bounded_int!(usize);

impl ToVal for f64 {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		Ok(Val::Flo(*self as f32))
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		Ok(Slot::Flo(*self as f32))
	}
}

impl ToVal for Num {
	#[inline(always)]
	fn to_val(&self) -> GResult<Val> {
		match *self {
			Num::Int(i) => Ok(Val::Int(i)),
			Num::Flo(f) => Ok(Val::Flo(f))
		}
	}

	#[inline(always)]
	fn to_slot(&self) -> GResult<Slot> {
		match *self {
			Num::Int(i) => Ok(Slot::Int(i)),
			Num::Flo(f) => Ok(Slot::Flo(f))
		}
	}
}

impl<T> ToVal for Vec<T> where for<'a> &'a T: ToVal {
	fn to_val(&self) -> GResult<Val> {
		let arr = glsp::arr_with_capacity(self.len());
		for t in self.iter() {
			arr.push(t)?
		}

		Ok(Val::Arr(arr))
	}
}

impl<T> ToVal for VecDeque<T> where for<'a> &'a T: ToVal {
	fn to_val(&self) -> GResult<Val> {
		let arr = glsp::arr_with_capacity(self.len());
		for t in self.iter() {
			arr.push(t)?
		}

		Ok(Val::Arr(arr))
	}
}

impl<A: smallvec::Array> ToVal for SmallVec<A> where for<'a> &'a A::Item: ToVal {
	fn to_val(&self) -> GResult<Val> {
		let arr = glsp::arr_with_capacity(self.len());
		for t in self.iter() {
			arr.push(t)?
		}

		Ok(Val::Arr(arr))
	}
}

impl<'a, T: ToVal> ToVal for &'a [T] {
	fn to_val(&self) -> GResult<Val> {
		let arr = glsp::arr_with_capacity(self.len());
		for t in self.iter() {
			arr.push(t)?
		}

		Ok(Val::Arr(arr))
	}
}

macro_rules! impl_to_val_array {
	($($len:literal),+) => (
		$(
			impl<T> ToVal for [T; $len] where for<'a> &'a T: ToVal {
				fn to_val(&self) -> GResult<Val> {
					let arr = glsp::arr_with_capacity($len);
					for t in self.iter() {
						arr.push(t)?
					}

					Ok(Val::Arr(arr))
				}
			}
		)+
	);
}

impl_to_val_array!(
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
	17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32
);

macro_rules! impl_to_val_tuple {
	($len:literal: $($t:ident $i:tt),+) => (
		impl<$($t),+> ToVal for ($($t,)+) 
		where 
			$( for<'a> &'a $t: ToVal ),+ 
		{
			fn to_val(&self) -> GResult<Val> {
				let arr = glsp::arr_with_capacity($len);

				$(
					arr.push(&(self.$i))?;
				)+

				Ok(Val::Arr(arr))
			}
		}
	);
}

impl_to_val_tuple!( 1: A 0);
impl_to_val_tuple!( 2: A 0, B 1);
impl_to_val_tuple!( 3: A 0, B 1, C 2);
impl_to_val_tuple!( 4: A 0, B 1, C 2, D 3);
impl_to_val_tuple!( 5: A 0, B 1, C 2, D 3, E 4);
impl_to_val_tuple!( 6: A 0, B 1, C 2, D 3, E 4, F 5);
impl_to_val_tuple!( 7: A 0, B 1, C 2, D 3, E 4, F 5, G 6);
impl_to_val_tuple!( 8: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
impl_to_val_tuple!( 9: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
impl_to_val_tuple!(10: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);
impl_to_val_tuple!(11: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10);
impl_to_val_tuple!(12: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10, L 11);

impl ToVal for String {
	fn to_val(&self) -> GResult<Val> {
		Ok(Val::Str(glsp::str_from_rust_str(&self)))
	}
}

impl ToVal for str {
	fn to_val(&self) -> GResult<Val> {
		Ok(Val::Str(glsp::str_from_rust_str(self)))
	}
}

impl ToVal for CString {
	fn to_val(&self) -> GResult<Val> {
		match self.to_str() {
			Ok(str_ref) => str_ref.to_val(),
			Err(_) => bail!("CString contained non-UTF-8 data")
		}
	}
}

impl ToVal for CStr {
	fn to_val(&self) -> GResult<Val> {
		match self.to_str() {
			Ok(str_ref) => str_ref.to_val(),
			Err(_) => bail!("CStr contained non-UTF-8 data")
		}
	}
}

impl ToVal for OsString {
	fn to_val(&self) -> GResult<Val> {
		match self.to_str() {
			Some(str_ref) => str_ref.to_val(),
			None => bail!("OsString contained non-UTF-8 data")
		}
	}
}

impl ToVal for OsStr {
	fn to_val(&self) -> GResult<Val> {
		match self.to_str() {
			Some(str_ref) => str_ref.to_val(),
			None => bail!("OsStr contained non-UTF-8 data")
		}
	}
}

impl ToVal for PathBuf {
	fn to_val(&self) -> GResult<Val> {
		self.as_os_str().to_val()
	}
}

impl ToVal for Path {
	fn to_val(&self) -> GResult<Val> {
		self.as_os_str().to_val()
	}
}

impl<K: ToVal, V: ToVal, S> ToVal for HashMap<K, V, S> {
	fn to_val(&self) -> GResult<Val> {
		let tab = glsp::tab_with_capacity(self.len());

		for (key, value) in self.iter() {
			let key_slot = key.to_slot()?;

			ensure!(!tab.has(&key_slot)?, "duplicate key in HashMap");
			tab.set(&key_slot, value)?;
		}

		Ok(Val::Tab(tab))
	}
}

impl<K: ToVal, V: ToVal> ToVal for BTreeMap<K, V> {
	fn to_val(&self) -> GResult<Val> {
		let tab = glsp::tab_with_capacity(self.len());

		for (key, value) in self.iter() {
			let key_slot = key.to_slot()?;

			ensure!(!tab.has(&key_slot)?, "duplicate key in BTreeMap");
			tab.set(&key_slot, value)?;
		}

		Ok(Val::Tab(tab))
	}
}


//-------------------------------------------------------------------------------------------------
// IntoResult
//-------------------------------------------------------------------------------------------------

/**
A type which can be returned from an `RFn`.

A blanket implementation is provided for any type which implements [`ToVal`](trait.ToVal.html), 
and also for any [`GResult<T>`](type.GResult.html) where `T: ToVal`.

It's not possible to implement this trait for your own types. Implement [`ToVal`](trait.ToVal.html) 
instead, or define your type using [`rdata!`](macro.rdata.html) or [`lib!`](macro.lib.html).
*/

pub trait IntoResult {
	#[doc(hidden)]
	fn into_result(self) -> GResult<Slot>;
}

//once specialization is enabled, we'll need to provide specialized impls for tuples, arrays, etc.
//we can't currently return collections of things which are passed by value, like RData.
impl<T> IntoResult for T where T: ToVal {
	fn into_result(self) -> GResult<Slot> {
		self.to_slot()
	}
}

//once specialization is enabled, we should add a generic impl for Result<T, E: Error>
impl<T> IntoResult for GResult<T> where T: IntoResult {
	fn into_result(self) -> GResult<Slot> {
		match self {
			Ok(payload) => payload.into_result(),
			Err(err) => Err(err)
		}
	}
}


//-------------------------------------------------------------------------------------------------
// FromVal implementations
//-------------------------------------------------------------------------------------------------

// Val, Slot
//-----------------------------------------------------------------------------

impl FromVal for Val {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		Ok(val.clone())
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		Ok(val.root())
	}
}

impl FromVal for Slot {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		Ok(Slot::from_val(val))
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		Ok(val.clone())
	}
}

// integers and other trivial Val fields
//-----------------------------------------------------------------------------

macro_rules! impl_from_val_infallible(
	($(($t:ty, $variant:ident)),+) => (
		$(
			impl FromVal for $t {
				#[inline(always)]
				fn from_val(val: &Val) -> GResult<Self> {
					match *val {
						Val::$variant(interior) => Ok(interior as $t),
						ref val => bail!("expected {}, received {}", 
						                 stringify!($t), val.a_type_name())
					}
				}

				#[inline(always)]
				fn from_slot(val: &Slot) -> GResult<Self> {
					match *val {
						Slot::$variant(interior) => Ok(interior as $t),
						ref val => bail!("expected {}, received {}", 
						                 stringify!($t), val.a_type_name())
					}
				}
			}
		)+
	);
);

impl_from_val_infallible!(
	(i32, Int),
	(i64, Int),
	(i128, Int),
	(isize, Int),
	(char, Char),
	(bool, Bool),
	(Sym, Sym),
	(RFn, RFn)
);

macro_rules! impl_from_val_root(
	($(($t:ty, $variant:ident)),+) => (
		$(
			impl FromVal for Root<$t> {
				#[inline(always)]
				fn from_val(val: &Val) -> GResult<Self> {
					match *val {
						Val::$variant(ref root) => Ok(root.clone()),
						ref val => bail!("expected {}, received {}", 
						                 stringify!(Root<$t>), val.a_type_name())
					}
				}

				#[inline(always)]
				fn from_slot(val: &Slot) -> GResult<Self> {
					match *val {
						Slot::$variant(ref gc) => Ok(gc.root()),
						ref val => bail!("expected {}, received {}", 
						                 stringify!(Root<$t>), val.a_type_name())
					}
				}
			}

			impl FromVal for Gc<$t> {
				#[inline(always)]
				fn from_val(val: &Val) -> GResult<Self> {
					match *val {
						Val::$variant(ref root) => Ok(root.as_gc().clone()),
						ref val => bail!("expected {}, received {}", 
						                 stringify!(Gc<$t>), val.a_type_name())
					}
				}

				#[inline(always)]
				fn from_slot(val: &Slot) -> GResult<Self> {
					match *val {
						Slot::$variant(ref gc) => Ok(gc.clone()),
						ref val => bail!("expected {}, received {}", 
						                 stringify!(Gc<$t>), val.a_type_name())
					}
				}
			}
		)+
	);
);

impl_from_val_root!(
	(Arr, Arr),
	(Str, Str),
	(Tab, Tab),
	(GIter, GIter),
	(Obj, Obj),
	(GFn, GFn),
	(Class, Class),
	(Coro, Coro),
	(RData, RData)
);

impl<T: RStore> FromVal for RRoot<T> {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<RRoot<T>> {
		match val {
			Val::RData(root) => Ok(RRoot::new(root.clone())),
			val => bail!("expected RRoot<{}>, received {}", type_name::<T>(), val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(slot: &Slot) -> GResult<RRoot<T>> {
		match slot {
			Slot::RData(gc) => Ok(RRoot::new(gc.root())),
			val => bail!("expected RRoot<{}>, received {}", type_name::<T>(), val.a_type_name())
		}
	}
}

macro_rules! impl_from_val_int_fallible_small(
	($($t:ident),+) => (
		$(
			impl FromVal for $t {
				#[inline(always)]
				fn from_val(val: &Val) -> GResult<Self> {
					match *val {
						Val::Int(i) if i >= $t::MIN as i32 && i <= $t::MAX as i32 => {
							Ok(i as $t)
						}
						Val::Int(i) => {
							bail!("expected {}, received an int with value {}",
							      stringify!($t), i)
						}
						ref val => bail!("expected {}, received {}", 
						                 stringify!($t), val.a_type_name())
					}
				}

				#[inline(always)]
				fn from_slot(val: &Slot) -> GResult<Self> {
					match *val {
						Slot::Int(i) if i >= $t::MIN as i32 && i <= $t::MAX as i32 => {
							Ok(i as $t)
						}
						Slot::Int(i) => {
							bail!("expected {}, received an int with value {}",
							      stringify!($t), i)
						}
						ref val => bail!("expected {}, received {}", 
						                 stringify!($t), val.a_type_name())
					}
				}
			}
		)+
	);
);

impl_from_val_int_fallible_small!(i8, i16, u8, u16);

macro_rules! impl_from_val_int_fallible_large(
	($($t:ty),+) => (
		$(
			impl FromVal for $t {
				#[inline(always)]
				fn from_val(val: &Val) -> GResult<Self> {
					match *val {
						Val::Int(i) if i >= 0 => {
							Ok(i as $t)
						}
						Val::Int(i) => {
							bail!("expected {}, received an int with value {}",
							      stringify!($t), i)
						}
						ref val => bail!("expected {}, received {}", 
						                 stringify!($t), val.a_type_name())
					}
				}

				#[inline(always)]
				fn from_slot(val: &Slot) -> GResult<Self> {
					match *val {
						Slot::Int(i) if i >= 0 => {
							Ok(i as $t)
						}
						Slot::Int(i) => {
							bail!("expected {}, received an int with value {}",
							      stringify!($t), i)
						}
						ref val => bail!("expected {}, received {}", 
						                 stringify!($t), val.a_type_name())
					}
				}
			}
		)+
	);
);

impl_from_val_int_fallible_large!(u32, u64, u128, usize);

// f32, f64
//-----------------------------------------------------------------------------

impl FromVal for f32 {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Flo(f) => Ok(f),
			ref val => bail!("expected f32, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		match *val {
			Slot::Flo(f) => Ok(f),
			ref val => bail!("expected f32, received {}", val.a_type_name())
		}
	}
}

impl FromVal for f64 {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Flo(f) => Ok(f as f64),
			ref val => bail!("expected f64, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		match *val {
			Slot::Flo(f) => Ok(f as f64),
			ref val => bail!("expected f64, received {}", val.a_type_name())
		}
	}
}

// Num
//-----------------------------------------------------------------------------

impl FromVal for Num {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Int(i) => Ok(Num::Int(i)),
			Val::Flo(f) => Ok(Num::Flo(f)),
			ref val => bail!("expected Num, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		match *val {
			Slot::Int(i) => Ok(Num::Int(i)),
			Slot::Flo(f) => Ok(Num::Flo(f)),
			ref val => bail!("expected Num, received {}", val.a_type_name())
		}
	}
}

// Deque
//-----------------------------------------------------------------------------

impl FromVal for Deque {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Arr(ref root) => Ok(Deque::Arr(root.clone())),
			Val::Str(ref root) => Ok(Deque::Str(root.clone())),
			ref val => bail!("expected Deque, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		match *val {
			Slot::Arr(ref gc) => Ok(Deque::Arr(gc.root())),
			Slot::Str(ref gc) => Ok(Deque::Str(gc.root())),
			ref val => bail!("expected Deque, received {}", val.a_type_name())
		}
	}
}

// Callable, Expander, Iterable, EnvMode
//-----------------------------------------------------------------------------

impl FromVal for Callable {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::GFn(ref root) => Ok(Callable::GFn(root.clone())),
			Val::RFn(rfn) => Ok(Callable::RFn(rfn)),
			Val::Class(ref root) => Ok(Callable::Class(root.clone())),
			ref val => bail!("expected Callable, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		match *val {
			Slot::GFn(ref gc) => Ok(Callable::GFn(gc.root())),
			Slot::RFn(rfn) => Ok(Callable::RFn(rfn)),
			Slot::Class(ref gc) => Ok(Callable::Class(gc.root())),
			ref val => bail!("expected Callable, received {}", val.a_type_name())
		}
	}
}

impl FromVal for Expander {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::GFn(ref root) => Ok(Expander::GFn(root.clone())),
			Val::RFn(rfn) => Ok(Expander::RFn(rfn)),
			ref val => bail!("expected Expander, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		match *val {
			Slot::GFn(ref gc) => Ok(Expander::GFn(gc.root())),
			Slot::RFn(rfn) => Ok(Expander::RFn(rfn)),
			ref val => bail!("expected Expander, received {}", val.a_type_name())
		}
	}
}

impl FromVal for Iterable {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match val {
			Val::Arr(root) => Ok(Iterable::Arr(root.clone())),
			Val::Str(root) => Ok(Iterable::Str(root.clone())),
			Val::Tab(root) => Ok(Iterable::Tab(root.clone())),
			Val::GIter(root) => Ok(Iterable::GIter(root.clone())),
			Val::Coro(root) => Ok(Iterable::Coro(root.clone())),
			val => bail!("expected Iterable, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(slot: &Slot) -> GResult<Self> {
		match slot {
			Slot::Arr(gc) => Ok(Iterable::Arr(gc.root())),
			Slot::Str(gc) => Ok(Iterable::Str(gc.root())),
			Slot::Tab(gc) => Ok(Iterable::Tab(gc.root())),
			Slot::GIter(gc) => Ok(Iterable::GIter(gc.root())),
			Slot::Coro(gc) => Ok(Iterable::Coro(gc.root())),
			slot => bail!("expected Iterable, received {}", slot.a_type_name())
		}
	}
}

impl FromVal for EnvMode {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Sym(sym) => {
				match sym {
					FRESH_SYM => Ok(EnvMode::Fresh),
					COPIED_SYM => Ok(EnvMode::Copied),
					_ => bail!("expected an EnvMode, received the symbol {}", sym)
				}
			}
			ref val => bail!("expected an EnvMode, received {}", val.a_type_name())
		}
	}
}

// Ordering
//-----------------------------------------------------------------------------

impl FromVal for Ordering {
	#[inline(always)]
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Sym(LT_SYM) => Ok(Ordering::Less),
			Val::Sym(NUM_EQ_SYM) => Ok(Ordering::Equal),
			Val::Sym(GT_SYM) => Ok(Ordering::Greater),
			ref val => bail!("expected Ordering, received {}", val.a_type_name())
		}
	}

	#[inline(always)]
	fn from_slot(val: &Slot) -> GResult<Self> {
		match *val {
			Slot::Sym(LT_SYM) => Ok(Ordering::Less),
			Slot::Sym(NUM_EQ_SYM) => Ok(Ordering::Equal),
			Slot::Sym(GT_SYM) => Ok(Ordering::Greater),
			ref val => bail!("expected Ordering, received {}", val.a_type_name())
		}
	}
}

// Vec<T>
//-----------------------------------------------------------------------------

impl<T: FromVal> FromVal for Vec<T> {
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Arr(ref arr) => {
				let mut vec = Vec::<T>::with_capacity(arr.len());

				let arr_borrow = arr.borrow();
				for slot in arr_borrow.iter() {
					vec.push(T::from_slot(slot)?);
				}

				Ok(vec)
			}
			ref val => bail!("expected a Vec, received {}", val.a_type_name())
		}
	}
}

// VecDeque<T>
//-----------------------------------------------------------------------------

impl<T: FromVal> FromVal for VecDeque<T> {
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Arr(ref arr) => {
				let mut vec = VecDeque::<T>::with_capacity(arr.len());

				let arr_borrow = arr.borrow();
				for slot in arr_borrow.iter() {
					vec.push_back(T::from_slot(slot)?);
				}

				Ok(vec)
			}
			ref val => bail!("expected a VecDeque, received {}", val.a_type_name())
		}
	}
}

// SmallVec<A>
//-----------------------------------------------------------------------------

impl<A> FromVal for SmallVec<A>
where
	A: smallvec::Array,
	A::Item: FromVal
{
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Arr(ref arr) => {
				let mut small_vec = SmallVec::<A>::with_capacity(arr.len());

				let arr_borrow = arr.borrow();
				for slot in arr_borrow.iter() {
					small_vec.push(A::Item::from_slot(slot)?);
				}

				Ok(small_vec)
			}
			ref val => bail!("expected a SmallVec, received {}", val.a_type_name())
		}
	}
}

// [T; n] where T: FromVal
//-----------------------------------------------------------------------------

macro_rules! impl_from_val_array {
	($($len:literal [$($n:literal),*]),+) => (
		$(
			impl<T> FromVal for [T; $len] where T: FromVal {
				fn from_val(val: &Val) -> GResult<[T; $len]> {
					match *val {
						Val::Arr(ref arr) => {
							ensure!(arr.len() == $len, 
							        "expected a [T; {}], received an arr of length {}",
							        $len, arr.len());

							Ok([$(
								arr.get::<T>($n)?,
							)*])
						}
						ref val => {
							bail!("expected a [T; {}], received {}", $len, val.a_type_name())
						}
					}
				}
			}
		)+
	);
}

impl_from_val_array!(
	0 [],
	1 [0], 
	2 [0, 1], 
	3 [0, 1, 2], 
	4 [0, 1, 2, 3], 
	5 [0, 1, 2, 3, 4], 
	6 [0, 1, 2, 3, 4, 5], 
	7 [0, 1, 2, 3, 4, 5, 6], 
	8 [0, 1, 2, 3, 4, 5, 6, 7], 
	9 [0, 1, 2, 3, 4, 5, 6, 7, 8], 
	10 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 
	11 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 
	12 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], 
	13 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], 
	14 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], 
	15 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], 
	16 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
);

// (T0, T1, ...) where T0: FromVal, T1: FromVal...
//-----------------------------------------------------------------------------

macro_rules! impl_from_val_tuple {
	($len:literal: $($t:ident $i:tt),+) => (
		impl<$($t),+> FromVal for ($($t,)+)
		where
			$($t: FromVal),+
		{
			fn from_val(val: &Val) -> GResult<($($t,)+)> {
				match *val {
					Val::Arr(ref arr) => {
						ensure!(arr.len() == $len, 
						        "expected a {}-element tuple, received an arr of length {}", 
						        $len, arr.len());

						Ok(($(
							arr.get::<$t>($i)?,
						)*))
					}
					ref val => bail!("expected a tuple, received {}", val.a_type_name())
				}
			}
		}
	);
}

impl_from_val_tuple!( 1: A 0);
impl_from_val_tuple!( 2: A 0, B 1);
impl_from_val_tuple!( 3: A 0, B 1, C 2);
impl_from_val_tuple!( 4: A 0, B 1, C 2, D 3);
impl_from_val_tuple!( 5: A 0, B 1, C 2, D 3, E 4);
impl_from_val_tuple!( 6: A 0, B 1, C 2, D 3, E 4, F 5);
impl_from_val_tuple!( 7: A 0, B 1, C 2, D 3, E 4, F 5, G 6);
impl_from_val_tuple!( 8: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
impl_from_val_tuple!( 9: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
impl_from_val_tuple!(10: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);
impl_from_val_tuple!(11: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10);
impl_from_val_tuple!(12: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10, L 11);

// String
//-----------------------------------------------------------------------------

impl FromVal for String {
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Str(ref st) => Ok(st.to_string()),
			ref val => bail!("expected a str, received {}", val.a_type_name())
		}
	}
}

// CString
//-----------------------------------------------------------------------------

impl FromVal for CString {
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Str(ref st) => {
				match CString::new(st.to_string()) {
					Ok(cstring) => Ok(cstring),
					Err(_) => {
						bail!("expected a C string, received a str with an inner nul")
					}
				}
			}
			ref val => bail!("expected a C string, received {}", val.a_type_name())
		}
	}
}

// PathBuf
//-----------------------------------------------------------------------------

impl FromVal for PathBuf {
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Str(ref st) => Ok(PathBuf::from(st.to_string())),
			ref val => bail!("expected a path, received {}", val.a_type_name())
		}
	}
}

// OsString
//-----------------------------------------------------------------------------

impl FromVal for OsString {
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Str(ref st) => Ok(OsString::from(st.to_string())),
			ref val => bail!("expected an OS string, received {}", val.a_type_name())
		}
	}
}

// HashMap<K, V>
//-----------------------------------------------------------------------------

impl<K, V, S> FromVal for HashMap<K, V, S>
where
	K: Hash + Eq + FromVal,
	V: FromVal,
	S: BuildHasher + Default
{
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Tab(ref tab) => {
				let s = S::default();
				let mut hash_map = HashMap::<K, V, S>::with_capacity_and_hasher(tab.len(), s);

				let tab_borrow = tab.borrow();
				for (internal_key, internal_value) in tab_borrow.iter() {
					let key = K::from_slot(internal_key)?;
					let value = V::from_slot(internal_value)?;
					
					if hash_map.insert(key, value).is_some() {
						bail!("duplicate key in HashMap argument");
					}
				}

				Ok(hash_map)
			}
			ref val => bail!("expected a HashMap, received {}", val.a_type_name())
		}
	}
}

// BTreeMap<K, V>
//-----------------------------------------------------------------------------

impl<K: Ord + FromVal, V: FromVal> FromVal for BTreeMap<K, V> {
	fn from_val(val: &Val) -> GResult<Self> {
		match *val {
			Val::Tab(ref tab) => {
				let mut btree_map = BTreeMap::<K, V>::new();

				let tab_borrow = tab.borrow();
				for (internal_key, internal_value) in tab_borrow.iter() {
					let key = K::from_slot(internal_key)?;
					let value = V::from_slot(internal_value)?;
					
					if btree_map.insert(key, value).is_some() {
						bail!("duplicate key in BTreeMap argument");
					}
				}

				Ok(btree_map)
			}
			ref val => bail!("expected a BTreeMap, received {}", val.a_type_name())
		}
	}
}


//-------------------------------------------------------------------------------------------------
// rfn!() and WrappedFn
//-------------------------------------------------------------------------------------------------

/**
Wrap a function pointer or closure so that it can be passed to [`glsp::rfn`](fn.rfn.html)
and similar functions.

The macro receives a single argument, which must be the path to a function, the path to a method,
or an expression which evaluates to a *non-capturing* closure.

The return value is a [`WrappedFn`](struct.WrappedFn.html). This is an opaque type: it doesn't
support any operations except being converted to an `RFn`.

In effect, this macro takes an arbitrary Rust function and converts it into a type-erased
function pointer which can be called by GameLisp. The function's return value must implement
[`IntoResult`](trait.IntoResult.html), and all of its arguments must implement
[`MakeArg`](trait.MakeArg.html).
*/

#[macro_export]
macro_rules! rfn {
	($fn_expr:expr) => (
		{
			$crate::WrappedFn::new(
				|vals: std::cell::Ref<[$crate::Slot]>| 
				 -> $crate::GResult<$crate::Slot> { 
				 	let mut temps = $crate::make_temps($fn_expr, &*vals)?;
				 	drop(vals);
					$crate::forwarder($fn_expr, &mut temps)
				},
				$crate::wrapped_arg_limits($fn_expr)
			)
		}
	);
}

/**
Data required to construct an `RFn`.

This opaque struct is produced by the [`rfn!` macro](macro.rfn.html), and consumed by 
[`glsp::rfn`](fn.rfn.html), [`glsp::bind_rfn`](fn.bind_rfn.html), and similar functions. 
*/

#[derive(Copy, Clone)]
pub struct WrappedFn {
	wrapper: fn(Ref<[Slot]>) -> GResult<Slot>,
	pub(crate) arg_limits: (usize, Option<usize>)
}

impl WrappedFn {
	#[doc(hidden)]
	pub fn new(wrapper: fn(Ref<[Slot]>) -> GResult<Slot>,
	           arg_limits: (usize, Option<usize>)) -> WrappedFn {
		WrappedFn {
			wrapper,
			arg_limits
		}
	}

	#[inline(always)]
	pub(crate) fn call(&self, vals: Ref<[Slot]>) -> GResult<Slot> {
		if vals.len() < self.arg_limits.0 {
			bail!("too few arguments: received {}, expected at least {}", 
			      vals.len(), self.arg_limits.0)
		}

		if let Some(max_args) = self.arg_limits.1 {
			if vals.len() > max_args {
				bail!("too many arguments: received {}, expected no more than {}",
				      vals.len(), max_args)
			}
		}

		(self.wrapper)(vals)
	}

	pub(crate) fn as_usize(&self) -> usize {
		self.wrapper as usize
	}
}


//-------------------------------------------------------------------------------------------------
// multiplexing tuple traits
//-------------------------------------------------------------------------------------------------

pub trait TupleCall<Args> {
	type Output;

	fn tuple_call(&self, args: Args) -> Self::Output;
}

pub trait MakeTemps {
	type Temps: 'static;

	fn make_temps(
		slots: &[Slot]
	) -> GResult<Self::Temps>;

	fn arg_limits() -> (usize, Option<usize>);
}

pub trait MakeArgs<'a>: Sized + MakeTemps {
	fn make_args(
		temps: &'a mut Self::Temps
	) -> GResult<Self>;
}

#[doc(hidden)]
#[inline(always)]
pub fn make_temps<Args, F>(
	_f: F, 
	vals: &[Slot]
) -> GResult<Args::Temps> 
where
	Args: MakeTemps,
	F: TupleCall<Args>
{
	Args::make_temps(vals)
}

#[doc(hidden)]
#[inline(always)]
pub fn forwarder<'a, Args, F>(
	f: F,
	temps: &'a mut Args::Temps
) -> GResult<Slot> 
where
	Args: MakeArgs<'a>,
	F: TupleCall<Args>,
	F::Output: IntoResult
{
	f.tuple_call(Args::make_args(temps)?).into_result()
}

#[doc(hidden)]
pub fn wrapped_arg_limits<Args, F>(_f: F) -> (usize, Option<usize>)
where
	Args: MakeTemps,
	F: TupleCall<Args>
{
	Args::arg_limits()
}

macro_rules! tuple_impls(
	($arg_count:literal; $($arg_type:ident),*; $($i:tt),*) => (

		impl<F, R $(,$arg_type)*> TupleCall<($($arg_type,)*)> for F
		where
			F: Fn($($arg_type),*) -> R
		{
			type Output = R;

			#[allow(unused_assignments, unused_mut, unused_variables)]
			#[inline(always)]
			fn tuple_call(&self, args: ($($arg_type,)*)) -> R {
				self($(args.$i),*)
			}
		}

		impl<'a $(,$arg_type)*> MakeTemps for ($($arg_type,)*)
		where
			$(
				$arg_type: MakeTemp
			),*
		{
			type Temps = ($($arg_type::Temp,)*);

			#[allow(unused_assignments, unused_mut, unused_variables)]
			#[inline(always)]
			fn arg_limits() -> (usize, Option<usize>) {
				let mut normal_args = 0;
				let mut opt_args = 0;
				let mut seen_rest = false;
				let mut seen_opt = false;
				let mut seen_glsp_or_lib = false;

				$(
					assert!(!seen_rest, "&[T] argument is somewhere other than final position");

					match $arg_type::ARG_TYPE {
						ArgType::Lib => {
							assert!(normal_args == 0 && opt_args == 0 && !seen_rest,
							        "&Lib argument appears after a normal argument");
						}
						ArgType::Normal => {
							assert!(!seen_opt, "Option<T> followed by a non-optional argument");
							normal_args += 1;
						}
						ArgType::Opt => {
							seen_opt = true;
							opt_args += 1;
						}
						ArgType::Rest => {
							seen_rest = true;
						}
					}
				)*

				(normal_args, if seen_rest { None } else { Some(normal_args + opt_args) })
			}

			#[allow(unused_assignments, unused_mut, unused_variables)]
			#[inline(always)]
			fn make_temps(
				vals: &[Slot]
			) -> GResult<($($arg_type::Temp,)*)> {
				let mut i = 0;
				Ok((
					$(
						{
							let temp = $arg_type::make_temp(vals, i)?;
							if $arg_type::ARG_TYPE != ArgType::Lib {
								i += 1;
							}
							temp
						}
					,)*
				))
			}
		}

		impl<'a $(,$arg_type)*> MakeArgs<'a> for ($($arg_type,)*)
		where
			$(
				$arg_type: MakeArg<'a>
			),*
		{
			#[allow(unused_assignments, unused_mut, unused_variables)]
			#[inline(always)]
			fn make_args(
			    temps: &'a mut Self::Temps
			) -> GResult<Self> {
				let mut i = 0;
				Ok((
					$(
						{
							let temp = $arg_type::make_arg(&mut temps.$i)?;
							if $arg_type::ARG_TYPE != ArgType::Lib {
								i += 1;
							}
							temp
						}
					,)*
				))
			}
		}

	);
);

//todo: ideally we would process Lib parameters last, so that arguments conversions using FromVal
//can access Libs. the ideal evaluation order would be make_temps, make_args, make_temps_libs,
//then make_args_libs, but there seems to be no straightforward way to achieve that, at least
//without risking poor performance or increasing compile times...

tuple_impls!(0; ; );
tuple_impls!(1; T0; 0);
tuple_impls!(2; T0, T1; 0, 1);
tuple_impls!(3; T0, T1, T2; 0, 1, 2);
tuple_impls!(4; T0, T1, T2, T3; 0, 1, 2, 3);
tuple_impls!(5; T0, T1, T2, T3, T4; 0, 1, 2, 3, 4);
tuple_impls!(6; T0, T1, T2, T3, T4, T5; 0, 1, 2, 3, 4, 5);
tuple_impls!(7; T0, T1, T2, T3, T4, T5, T6; 0, 1, 2, 3, 4, 5, 6);
tuple_impls!(8; T0, T1, T2, T3, T4, T5, T6, T7;
             0, 1, 2, 3, 4, 5, 6, 7);
tuple_impls!(9; T0, T1, T2, T3, T4, T5, T6, T7, T8;
             0, 1, 2, 3, 4, 5, 6, 7, 8);
tuple_impls!(10; T0, T1, T2, T3, T4, T5, T6, T7, T8, T9;
             0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
tuple_impls!(11; T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10;
             0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
tuple_impls!(12; T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11;
             0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);


//-------------------------------------------------------------------------------------------------
// MakeTemp and MakeArg, and their implementations
//-------------------------------------------------------------------------------------------------

#[doc(hidden)]
#[derive(PartialEq)]
pub enum ArgType {
	Lib,
	Normal,
	Opt,
	Rest
}

#[doc(hidden)]
pub trait MakeTemp {
	const ARG_TYPE: ArgType = ArgType::Normal;
	type Temp: 'static;
	fn make_temp(
		vals: &[Slot], 
		i: usize
	) -> GResult<Self::Temp>;
}

/**
A type which can act as an `RFn` parameter.

A blanket implementation is provided for any type which implements [`FromVal`](trait.FromVal.html), 
and also for several other types:
	
- `Option<T>`, which acts as an optional parameter, storing `None` when an argument isn't
  provided.
- `&[T]` or `&mut [T]`, which act as a "rest" parameter, capturing any number of arguments.
- [`OrNil<T>`](struct.OrNil.html), which accepts either the specified type or `#n`.
- Shared references to primitive types: [`&Arr`](struct.Arr.html), [`&Tab`](struct.Tab.html), etc.
- String slices: `&str`, `&Path`, `&CStr`, `&OsStr`.
- `&T` and `&mut T`, where `T` was defined using the [`lib!`](macro.lib.html) or
  [`rdata!`](macro.rdata.html) macros.

It's not possible to implement this trait for your own types. Implement 
[`FromVal`](trait.FromVal.html) instead, or define your type using [`rdata!`](macro.rdata.html)
or [`lib!`](macro.lib.html).
*/

pub trait MakeArg<'a>: Sized + MakeTemp {
	fn make_arg(
		temp: &'a mut Self::Temp
	) -> GResult<Self>;
}

impl<T> MakeTemp for T where T: FromVal {
	type Temp = Slot;

	#[inline(always)]
	fn make_temp(
		vals: &[Slot], 
		i: usize
	) -> GResult<Slot> {
		Ok(vals[i].clone())
	}
}

//note that the specialization RFC implies that &'a T is supposed to be more-specific than T,
//even though this isn't the case with today's feature(specialization). once this is the case,
//we will be able to specialize this impl for &T where T: FromVal etc.

impl<'a, T> MakeArg<'a> for T where T: Sized + FromVal + MakeTemp<Temp = Slot> {
	#[inline(always)]
	fn make_arg(
		temp: &'a mut Slot
	) -> GResult<T> {
		T::from_slot(&temp)
	}
}

// Option<T>
//-----------------------------------------------------------------------------

impl<T> MakeTemp for Option<T> where T: MakeTemp {
	const ARG_TYPE: ArgType = ArgType::Opt;

	type Temp = Option<T::Temp>;

	#[inline(always)]
	fn make_temp(
		vals: &[Slot], 
		i: usize
	) -> GResult<Option<T::Temp>> {
		assert!(T::ARG_TYPE == ArgType::Normal, "invalid Option<T> argument in rfn");

		if i < vals.len() {
			Ok(Some(T::make_temp(vals, i)?))
		} else {
			Ok(None)
		}
	}
}

impl<'a, T> MakeArg<'a> for Option<T> where T: MakeArg<'a> {
	#[inline(always)]
	fn make_arg(
		temp: &'a mut Option<T::Temp>
	) -> GResult<Option<T>> {
		match *temp {
			Some(ref mut temp) => Ok(Some(T::make_arg(temp)?)),
			None => Ok(None)
		}
	}
}

// OrNil<T>
//-----------------------------------------------------------------------------

/**
A wrapper type for an `RFn` argument which allows it to be `#n`.

For example, this function could be called as either `(example 10)` or `(example #n)`:
	
	fn example(OrNil(i): OrNil<i32>) {
		if let Some(i) = i {
			println!("{}", i);
		}
	}
*/

pub struct OrNil<T>(pub Option<T>);

impl<T> MakeTemp for OrNil<T> where T: MakeTemp {
	type Temp = Option<T::Temp>;

	#[inline(always)]
	fn make_temp(
		vals: &[Slot], 
		i: usize
	) -> GResult<Option<T::Temp>> {
		assert!(T::ARG_TYPE == ArgType::Normal, "invalid OrNil<T> argument in rfn");

		if let Slot::Nil = vals[i] {
			Ok(None)
		} else {
			Ok(Some(T::make_temp(vals, i)?))
		}
	}
}

impl<'a, T> MakeArg<'a> for OrNil<T> where T: MakeArg<'a> {
	#[inline(always)]
	fn make_arg(
		temp: &'a mut Option<T::Temp>
	) -> GResult<OrNil<T>> {
		match temp {
			Some(ref mut temp) => Ok(OrNil(Some(T::make_arg(temp)?))),
			None => Ok(OrNil(None))
		}
	}
}

// &[T]
//-----------------------------------------------------------------------------

impl<'r, T> MakeTemp for &'r [T]
where
	T: 'static + for<'a> MakeArg<'a, Temp = Slot>,
	[T; 8]: smallvec::Array<Item = T>
{
	const ARG_TYPE: ArgType = ArgType::Rest;
	type Temp = SmallVec<[T; 8]>;

	#[inline(always)]
	fn make_temp(
		vals: &[Slot], 
		i: usize
	) -> GResult<SmallVec<[T; 8]>> {
		GResult::<SmallVec<[T; 8]>>::from_iter((i .. vals.len()).map(|j| {
			let mut slot = vals[j].clone();
			T::make_arg(&mut slot)
		}))
	}
}

impl<'a: 'r, 'r, T> MakeArg<'a> for &'r [T]
where
	T: 'static + for<'a2> MakeArg<'a2, Temp = Slot>,
	[T; 8]: smallvec::Array<Item = T>
{
	#[inline(always)]
	fn make_arg(
		temp: &'a mut SmallVec<[T; 8]>
	) -> GResult<&'r [T]> {
		Ok(&temp[..])
	}
}

// &mut [T]
//-----------------------------------------------------------------------------

impl<'r, T> MakeTemp for &'r mut [T]
where
	T: 'static + for<'a> MakeArg<'a, Temp = Slot>,
	[T; 8]: smallvec::Array<Item = T>
{
	const ARG_TYPE: ArgType = ArgType::Rest;
	type Temp = SmallVec<[T; 8]>;

	#[inline(always)]
	fn make_temp(
		vals: &[Slot], 
		i: usize
	) -> GResult<SmallVec<[T; 8]>> {
		GResult::<SmallVec<[T; 8]>>::from_iter((i .. vals.len()).map(|j| {
			let mut slot = vals[j].clone();
			T::make_arg(&mut slot)
		}))
	}
}

impl<'a: 'r, 'r, T> MakeArg<'a> for &'r mut [T]
where
	T: 'static + for<'a2> MakeArg<'a2, Temp = Slot>,
	[T; 8]: smallvec::Array<Item = T>
{
	#[inline(always)]
	fn make_arg(
		temp: &'a mut SmallVec<[T; 8]>
	) -> GResult<&'r mut [T]> {
		Ok(&mut temp[..])
	}
}

// &Arr, &Str, etc.
//-----------------------------------------------------------------------------

macro_rules! impl_pointee_make_arg {
	($(($pointee:ident, $variant:ident)),+) => (
		$(
			impl<'r> MakeTemp for &'r $pointee {
				type Temp = Slot;

				#[inline(always)]
				fn make_temp(
					vals: &[Slot], 
					i: usize
				) -> GResult<Slot> {
					Ok(vals[i].clone())
				}
			}

			impl<'a: 'r, 'r> MakeArg<'a> for &'r $pointee {
				#[inline(always)]
				fn make_arg(
					temp: &'a mut Slot
				) -> GResult<&'r $pointee> {
					match *temp {
						Slot::$variant(ref gc) => Ok(&**gc),
						ref val => bail!("expected &{}, received {}", 
						                 stringify!($pointee), (val.type_name()))
					}
				}
			}
		)+
	);
}

impl_pointee_make_arg!(
	(Arr, Arr),
	(Str, Str),
	(Tab, Tab),
	(GIter, GIter),
	(GFn, GFn),
	(Obj, Obj),
	(Class, Class),
	(Coro, Coro),
	(RData, RData)
);

// &str, &Path, &CStr, &OsStr
//-----------------------------------------------------------------------------

impl<'r> MakeTemp for &'r str {
	type Temp = SmallVec<[u8; 128]>;

	#[inline(always)]
	fn make_temp(
		vals: &[Slot], 
		i: usize
	) -> GResult<SmallVec<[u8; 128]>> {

		let mut vec = SmallVec::<[u8; 128]>::new();

		match vals[i] {
			Slot::Str(ref st) => write!(&mut vec, "{}", st).unwrap(),
			ref val => bail!("expected a &str, received {}", val.a_type_name())
		}

		Ok(vec)
	}
}

impl<'a: 'r, 'r> MakeArg<'a> for &'r str {
	#[inline(always)]
	fn make_arg(
		temp: &'a mut SmallVec<[u8; 128]>
	) -> GResult<&'r str> {
		Ok(str::from_utf8(&temp[..]).unwrap())
	}
}

macro_rules! impl_make_arg_text_slice (
	($(($slice_type:ident, $owned_type:ident)),+) => (
		$(
			impl<'r> MakeTemp for &'r $slice_type {
				type Temp = $owned_type;

				#[inline(always)]
				fn make_temp(
					vals: &[Slot], 
					i: usize
				) -> GResult<$owned_type> {
					let mut slot = vals[i].clone();
					$owned_type::make_arg(&mut slot)
				}
			}

			impl<'a: 'r, 'r> MakeArg<'a> for &'r $slice_type {
				#[inline(always)]
				fn make_arg(
					temp: &'a mut $owned_type
				) -> GResult<&'r $slice_type> {
					Ok(&**temp)
				}
			}
		)+
	);
);

impl_make_arg_text_slice!(
	(Path, PathBuf),
	(CStr, CString),
	(OsStr, OsString)
);


//-------------------------------------------------------------------------------------------------
// lib!, rdata!
//-------------------------------------------------------------------------------------------------

/**
Defines a library type.

The input must be a struct or enum declaration. The macro defines that type, implements the 
[`Lib` trait](trait.Lib.html) for the type, and implements [`MakeArg`](trait.MakeArg.html) 
for shared and mutable references to the type.

When a reference to a library struct is bound as an `RFn` parameter, that parameter won't 
consume any input arguments. Instead, it will attempt to [borrow](trait.Lib.html#method.borrow) 
the library struct from the active `Runtime`.
	
	lib! {
		struct Graphics {
			canvas: sdl2::render::Canvas<Window>
		}
	}

	impl Graphics {
		fn draw_rect(&self, rect: Rect) {
			self.canvas.draw_rect(rect).unwrap();
		}
	}

	glsp::bind_rfn("draw-rect", rfn!(Graphics::draw_rect))?;
*/

#[macro_export]
macro_rules! lib {

	//struct Name { ... }
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $lib:ident { $($struct_token:tt)* }
	) => (
		$(#[$struct_attr])*
		$struct_vis struct $lib { $($struct_token)* }

		$crate::lib_impls! { $lib }
	);

	//struct Name;
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $lib:ident;
	) => (
		$(#[$struct_attr])*
		$struct_vis struct $lib;

		$crate::lib_impls! { $lib }
	);

	//struct Name(...)
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $lib:ident ( $($struct_token:tt)* );
	) => (
		$(#[$struct_attr])*
		$struct_vis struct $lib ( $($struct_token)* );

		$crate::lib_impls! { $lib }
	);

	//enum Name { ... }
	(
		$(#[$enum_attr:meta])*
		$enum_vis:vis enum $lib:ident { $($enum_token:tt)* }
	) => (
		$(#[$enum_attr])*
		$enum_vis enum $lib { $($enum_token)* }

		$crate::lib_impls! { $lib }
	);
}

#[doc(hidden)]
#[macro_export]
macro_rules! lib_impls {
	($lib:ident) => (
		impl $crate::Lib for $lib {
			fn type_name() -> &'static str { 
				stringify!($lib) 
			}
		}

		impl<'r> $crate::MakeTemp for &'r $lib {
			const ARG_TYPE: $crate::ArgType = $crate::ArgType::Lib;
			type Temp = $crate::LibRef<$lib>;

			#[inline(always)]
			fn make_temp(
				_vals: &[$crate::Slot], 
				_i: usize
			) -> $crate::GResult<$crate::LibRef<$lib>> {
				$crate::try_lib::<$lib>()
			}
		}

		impl<'a: 'r, 'r> $crate::MakeArg<'a> for &'r $lib {
			#[inline(always)]
			fn make_arg(
				temp: &'a mut $crate::LibRef<$lib>
			) -> $crate::GResult<&'r $lib> {
				Ok(&**temp)
			}
		}

		impl<'r> $crate::MakeTemp for &'r mut $lib {
			const ARG_TYPE: $crate::ArgType = $crate::ArgType::Lib;
			type Temp = $crate::LibRefMut<$lib>;

			#[inline(always)]
			fn make_temp(
				_vals: &[$crate::Slot], 
				_i: usize
			) -> $crate::GResult<$crate::LibRefMut<$lib>> {
				$crate::try_lib_mut::<$lib>()
			}
		}

		impl<'a: 'r, 'r> $crate::MakeArg<'a> for &'r mut $lib {
			#[inline(always)]
			fn make_arg(
				temp: &'a mut $crate::LibRefMut<$lib>
			) -> $crate::GResult<&'r mut $lib> {
				Ok(&mut **temp)
			}
		}
	);
}

/**
Defines a Rust type which can be stored on the garbage-collected heap.

The input must be a struct or enum declaration, optionally followed by a `meths { ... }` block. 

The macro declares the specified type, implements the [`RStore` trait](trait.RStore.html) trait 
for the type, implements [`MakeArg`](trait.MakeArg.html) for shared and mutable references to 
the type, and implements [`IntoResult`](trait.IntoResult.html) for the type itself.

When a reference to an `rdata!` type is bound as an `RFn` parameter, that parameter expects
an argument which belongs to the [`rdata` primitive type](struct.RData.html). The argument will
be [borrowed](struct.RData.html#method.borrow) for the duration of the function call.

	rdata! {
		#[derive(Clone)]
		struct AudioClip {
			samples: Vec<i16>,
			channels: Channels
		}

		meths {
			get "duration": AudioClip::duration,
			"play": AudioClip::play
		}
	}

	impl AudioClip {
		fn load<P: AsRef<Path>>(path: P) -> AudioClip {
			//...
		}

		fn duration(&self) -> usize {
			self.samples.len() / self.channels.count()
		}

		fn play(&self, mixer: &mut Mixer) {
			mixer.play_audio_clip(self);
		}
	}

	glsp::bind_rfn("AudioClip:load", AudioClip::load::<PathBuf>)?;

## Methods

When a type is declared using the `rdata!` macro, it can be associated with named methods.
These methods can be called directly from GameLisp code. Alternatively, they can be called from
Rust code - for example, using [`RData::call`](struct.RData.html#method.call).

The `meths` block contains a comma-separated list of `"name": fn_expr` pairs. Each `fn_expr` 
is passed to the [`rfn!`](macro.rfn.html) macro, and the resulting 
[`WrappedFn`](struct.WrappedFn.html) is made into an associated method.

Each `"name"` can be prefixed with the keyword `get` or `set` to bind it as a property getter
or property setter, respectively.

## Limitations

Types defined using the `rdata!` macro must follow a number of restrictions. Most of these
restrictions are checked automatically, at compile time.

The type declaration must not have any generic parameters, and it must not have a `where` clause.

The type must be `'static`. GameLisp currently provides no mechanism for moving borrowed Rust
data onto the garbage-collected heap.

The type may not contain any [`Root`](struct.Root.html)s. This means that `RData` cannot directly
point to other `RData`. Instead, consider storing your `Root` in a [library](macro.lib.html)
or a [global variable](fn.bind_global.html). Other `RData` can then refer to the `Root` 
indirectly - perhaps using an integer to index an array, or using a symbol to access a hash table.

The type's unprefixed name must be unique. If you define two `rdata!` types, one named 
`audio::Clip` and one named `video::Clip`, then an error will occur when you call 
[`glsp::rdata`](fn.rdata.html).
*/

#[macro_export]
macro_rules! rdata {

	//struct Name { ... }
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $rdata:ident { $($struct_token:tt)* }

		$($rest:tt)*
	) => (
		$(#[$struct_attr])*
		$struct_vis struct $rdata { $($struct_token)* }

		$crate::rdata_impls! { $rdata; $($rest)* }
	);

	//struct Name;
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $rdata:ident;
		$($rest:tt)*
	) => (
		$(#[$struct_attr])*
		$struct_vis struct $rdata;

		$crate::rdata_impls! { $rdata; $($rest)* }
	);

	//struct Name(...)
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis struct $rdata:ident ( $($struct_token:tt)* );
		$($rest:tt)*
	) => (
		$(#[$struct_attr])*
		$struct_vis struct $rdata ( $($struct_token)* );

		$crate::rdata_impls! { $rdata; $($rest)* }
	);

	//enum Name { ... }
	(
		$(#[$enum_attr:meta])*
		$enum_vis:vis enum $rdata:ident { $($enum_token:tt)* }
		$($rest:tt)*
	) => (
		$(#[$enum_attr])*
		$enum_vis enum $rdata { $($enum_token)* }

		$crate::rdata_impls! { $rdata; $($rest)* }
	);
}

#[doc(hidden)]
#[macro_export]
macro_rules! rdata_impls {
	($rdata:ident;) => (rdata_impls!($rdata; meths { }););

	(
		$rdata:ident;

		meths { 
			$($($meth_kind:ident)? $meth_name:literal : $meth_expr:path,)+
		}
	) => (
		rdata_impls!(
			$rdata;

			meths {
				$($($meth_kind)? $meth_name: $meth_expr),+
			}
		);
	);

	(
		$rdata:ident;

		meths { 
			$($($meth_kind:ident)? $meth_name:literal : $meth_expr:path),*
		}
	) => (
		impl $crate::RStore for $rdata {
			fn type_name() -> &'static str { 
				stringify!($rdata) 
			}

			fn size_of() -> usize {
				std::mem::size_of::<$rdata>()
			}

			fn rclass() -> $crate::GResult<$crate::RClass> {
				$crate::RClass::from_vec(
					stringify!($rdata),
					std::vec![
						$((stringify!($($meth_kind)?), $meth_name, $crate::rfn!($meth_expr))),*
					]
				)
			}
		}

		impl<'r> $crate::MakeTemp for &'r $rdata {
			type Temp = $crate::RRef<$rdata>;

			#[inline(always)]
			fn make_temp(
				vals: &[$crate::Slot], 
				i: usize
			) -> $crate::GResult<$crate::RRef<$rdata>> {
				match vals[i] {
					$crate::Slot::RData(ref rdata) => rdata.try_borrow(),
					ref val => bail!("expected {}, received {}", 
					                 <$rdata as $crate::RStore>::type_name(), 
					                 val.a_type_name())
				}
			}
		}

		impl<'a: 'r, 'r> $crate::MakeArg<'a> for &'r $rdata {
			#[inline(always)]
			fn make_arg(
				temp: &'a mut $crate::RRef<$rdata>
			) -> $crate::GResult<&'r $rdata> {
				Ok(&**temp)
			}
		}

		impl<'r> $crate::MakeTemp for &'r mut $rdata {
			type Temp = $crate::RRefMut<$rdata>;

			#[inline(always)]
			fn make_temp(
				vals: &[$crate::Slot], 
				i: usize
			) -> $crate::GResult<$crate::RRefMut<$rdata>> {
				match vals[i] {
					$crate::Slot::RData(ref rdata) => rdata.try_borrow_mut(),
					ref val => bail!("expected {}, received {}", 
					                 <$rdata as $crate::RStore>::type_name(), 
					                 val.a_type_name())
				}
			}
		}

		impl<'a: 'r, 'r> $crate::MakeArg<'a> for &'r mut $rdata {
			#[inline(always)]
			fn make_arg(
				temp: &'a mut $crate::RRefMut<$rdata>
			) -> $crate::GResult<&'r mut $rdata> {
				Ok(&mut **temp)
			}
		}

		impl $crate::IntoResult for $rdata {
			fn into_result(self) -> GResult<$crate::Slot> {
				Ok($crate::Slot::RData($crate::rdata(self)?.into_gc()))
			}
		}
	);
}


//-------------------------------------------------------------------------------------------------
// Callable, CallableOps, ToCallArgs
//-------------------------------------------------------------------------------------------------

/**
A type-erased `callable`.

Because this type implements the [`CallableOps` trait](trait.CallableOps.html), you can call 
it directly, without needing to access the underlying types.
*/

#[derive(Clone, Debug)]
pub enum Callable {
	RFn(RFn),
	GFn(Root<GFn>),
	Class(Root<Class>)
}

/**
The `callable` abstract type.

[`glsp:call`](fn.call.html) can be used to call any type which implements this trait.

This trait is [sealed]. It's not possible to implement this trait for your own types.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait CallableOps: callable_ops_private::Sealed {
	#[doc(hidden)]
	fn receive_call(&self, arg_count: usize) -> GResult<Val>;

	///Returns this function's registered name, if any.
	fn name(&self) -> Option<Sym>;

	///Returns this function's minimum and maximum argument count.
	fn arg_limits(&self) -> (usize, Option<usize>);

	///Returns this function's minimum argument count.
	fn min_args(&self) -> usize {
		self.arg_limits().0
	}

	///Returns this function's maximum argument count, if any.
	fn max_args(&self) -> Option<usize> {
		self.arg_limits().1
	}
}

mod callable_ops_private {
	use crate::{class::Class, code::GFn, engine::RFn, gc::{Gc, Root}, wrap::Callable}; 

	pub trait Sealed { }

	impl Sealed for Callable { }
	impl Sealed for RFn { }
	impl Sealed for Root<Class> { }
	impl Sealed for Gc<Class> { }
	impl Sealed for Root<GFn> { }
	impl Sealed for Gc<GFn> { }
}

impl CallableOps for Callable {
	fn receive_call(&self, arg_count: usize) -> GResult<Val> {
		match *self {
			Callable::RFn(rfn) => glsp::call_rfn(rfn, arg_count).map(|slot| slot.root()),
			Callable::GFn(ref gfn_root) => glsp::call_gfn(gfn_root, arg_count),
			Callable::Class(ref class_root) => Ok(Val::Obj(glsp::call_class(class_root, arg_count)?))
		}
	}

	fn arg_limits(&self) -> (usize, Option<usize>) {
		match *self {
			Callable::RFn(rfn) => rfn.arg_limits(),
			Callable::GFn(ref gfn_root) => gfn_root.arg_limits(),
			Callable::Class(ref class_root) => class_root.arg_limits()
		}
	}

	fn name(&self) -> Option<Sym> {
		match *self {
			Callable::RFn(rfn) => rfn.name(),
			Callable::GFn(ref gfn_root) => gfn_root.name(),
			Callable::Class(ref class_root) => class_root.name()
		}
	}
}

/**
A type which can be converted into the arguments to a function call.

It's not possible to implement this trait for your own types, but it's implemented for tuples
and vectors of various sizes, when their elements all implement [`ToVal`](trait.ToVal.html).

Functions like [`glsp:call`](fn.call.html) and [`Obj::call`](struct.Obj.html#method.call) are
generic over this trait. They usually define their arguments as `&T where T: ToCallArgs`,
so tuples of arguments will need to be passed by reference:
	
	let push_rfn: RFn = glsp::global("push!");
	glsp::call(&push_rfn, &(my_arr, 100i32))?;
*/

pub trait ToCallArgs: to_call_args_private::Sealed {
	fn arg_count(&self) -> usize;
	fn to_call_args<E: Extend<Slot>>(&self, dst: &mut E) -> GResult<()>;
}

mod to_call_args_private {
	use crate::{wrap::ToVal};

	pub trait Sealed { }

	impl<T: ToVal> Sealed for [T] { }
	impl<T> Sealed for [T; 0] { }
	impl Sealed for () { }
}

impl<T: ToVal> ToCallArgs for [T] {
	fn arg_count(&self) -> usize {
		self.len()
	}

	fn to_call_args<E: Extend<Slot>>(&self, dst: &mut E) -> GResult<()> {
		let mut result = Ok(());
		dst.extend(self.iter().map(|item| {
			match item.to_slot() {
				Ok(slot) => slot,
				Err(err) => {
					result = Err(err);
					Slot::Nil
				}
			}
		}));
		result
	}
}

impl<T> ToCallArgs for [T; 0] {
	fn arg_count(&self) -> usize {
		0
	}
	
	fn to_call_args<E: Extend<Slot>>(&self, _dst: &mut E) -> GResult<()> {
		Ok(())
	}
}

macro_rules! impl_to_call_args_array {
	($len:literal) => (
		impl<T: ToVal> to_call_args_private::Sealed for [T; $len] { }

		impl<T: ToVal> ToCallArgs for [T; $len] {
			fn arg_count(&self) -> usize {
				$len
			}
			
			fn to_call_args<E: Extend<Slot>>(&self, dst: &mut E) -> GResult<()> {
				let mut result = Ok(());
				dst.extend(self.iter().map(|item| {
					match item.to_slot() {
						Ok(slot) => slot,
						Err(err) => {
							result = Err(err);
							Slot::Nil
						}
					}
				}));
				result
			}
		}
	)
}

impl_to_call_args_array!(1);
impl_to_call_args_array!(2);
impl_to_call_args_array!(3);
impl_to_call_args_array!(4);
impl_to_call_args_array!(5);
impl_to_call_args_array!(6);
impl_to_call_args_array!(7);
impl_to_call_args_array!(8);
impl_to_call_args_array!(9);
impl_to_call_args_array!(10);
impl_to_call_args_array!(11);
impl_to_call_args_array!(12);

impl ToCallArgs for () {
	fn arg_count(&self) -> usize {
		0
	}
	
	fn to_call_args<E: Extend<Slot>>(&self, _dst: &mut E) -> GResult<()> {
		Ok(())
	}
}

macro_rules! impl_to_call_args_tuple {
	($len:literal: $($t:ident $i:tt),+) => (
		impl<$($t),+> to_call_args_private::Sealed for ($($t,)+) where $( $t: ToVal ),+ { }

		impl<$($t),+> ToCallArgs for ($($t,)+)
		where 
			$( $t: ToVal ),+ 
		{
			fn arg_count(&self) -> usize {
				$len
			}
			
			fn to_call_args<EE: Extend<Slot>>(&self, dst: &mut EE) -> GResult<()> {
				let slots = [ $(
					(self.$i).to_slot()?
				),+ ];

				dst.extend(slots.iter().cloned());
				Ok(())
			}
		}
	);
}

impl_to_call_args_tuple!( 1: A 0);
impl_to_call_args_tuple!( 2: A 0, B 1);
impl_to_call_args_tuple!( 3: A 0, B 1, C 2);
impl_to_call_args_tuple!( 4: A 0, B 1, C 2, D 3);
impl_to_call_args_tuple!( 5: A 0, B 1, C 2, D 3, E 4);
impl_to_call_args_tuple!( 6: A 0, B 1, C 2, D 3, E 4, F 5);
impl_to_call_args_tuple!( 7: A 0, B 1, C 2, D 3, E 4, F 5, G 6);
impl_to_call_args_tuple!( 8: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
impl_to_call_args_tuple!( 9: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
impl_to_call_args_tuple!(10: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);
impl_to_call_args_tuple!(11: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10);
impl_to_call_args_tuple!(12: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10, L 11);
