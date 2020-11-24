use fnv::{FnvHashMap};
use smallvec::{SmallVec};
use std::{u8, u16, char};
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::cmp::{Ordering};
use std::collections::{hash_map, VecDeque};
use std::convert::{TryFrom};
use std::default::{Default};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, FusedIterator, repeat};
use std::marker::{PhantomData};
use std::mem::{size_of};
use std::ops::{Bound, RangeBounds};
use super::engine::{glsp, Guard, Span, with_heap};
use super::error::{GError, GResult};
use super::gc::{Allocate, GcHeader, Slot, Root, Visitor};
use super::iter::{GIter, GIterState};
use super::val::{Val};
use super::wrap::{FromVal, ToVal};


//-------------------------------------------------------------------------------------------------
// DequeIndex
//-------------------------------------------------------------------------------------------------

/**
An integer type which can be used as a deque index.

The [`DequeAccess`](trait.DequeAccess.html) trait is generic over this trait. This enables 
[`Arrs`](struct.Arr.html), [`Strs`](struct.Str.html) and [`Deques`](enum.Deque.html) to be
indexed using any primitive integer type, including negative indexes.

This trait is [sealed]. It's not possible to implement this trait for your own types.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait DequeIndex: deque_index_private::Sealed {
	//converts to usize, subtracts negative values from arr.len(), and checks that the result is
	//in the range 0 .. arr.len()
	#[doc(hidden)]
	fn as_usize<A>(&self, arr: &A) -> GResult<usize> where A: DequeOps;

	//as above, but tests against the range 0 ..= arr.len() instead. used by DequeRange, below
	#[doc(hidden)]
	fn as_usize_excluded<A: DequeOps>(&self, arr: &A) -> GResult<usize>;
}

mod deque_index_private {
	pub trait Sealed { }

	impl Sealed for u8 { }
	impl Sealed for u16 { }
	impl Sealed for u32 { }
	impl Sealed for u64 { }
	impl Sealed for u128 { }
	impl Sealed for usize { }
	impl Sealed for i8 { }
	impl Sealed for i16 { }
	impl Sealed for i32 { }
	impl Sealed for i64 { }
	impl Sealed for i128 { }
	impl Sealed for isize { }
}

macro_rules! impl_arr_index_unsigned {
	($type:ty) => (
		impl DequeIndex for $type {
			#[inline(always)]
			fn as_usize<A>(&self, arr: &A) -> GResult<usize> where A: DequeOps {
				ensure!((*self as usize) < arr.len(), 
				        "out-of-bounds arr access: len is {}, index is {}",
				        arr.len(), *self);
				Ok(*self as usize)
			}

			#[inline(always)]
			fn as_usize_excluded<A>(&self, arr: &A) -> GResult<usize> where A: DequeOps {
				ensure!((*self as usize) <= arr.len(), 
				        "out-of-bounds arr access: len is {}, index is {}",
				        arr.len(), *self);
				Ok(*self as usize)
			}
		}
	);
}

impl_arr_index_unsigned!(u8);
impl_arr_index_unsigned!(u16);
impl_arr_index_unsigned!(u32);
impl_arr_index_unsigned!(u64);
impl_arr_index_unsigned!(u128);
impl_arr_index_unsigned!(usize);

macro_rules! impl_arr_index_signed {
	($type:ty) => (
		impl DequeIndex for $type {
			#[inline(always)]
			fn as_usize<A>(&self, arr: &A) -> GResult<usize> where A: DequeOps {
				if *self >= 0 {
					ensure!((*self as usize) < arr.len(), 
					        "out-of-bounds arr access: len is {}, index is {}", arr.len(), *self);
					Ok(*self as usize)
				} else {
					let index = arr.len() as isize + (*self as isize);
					ensure!(index >= 0 && (index as usize) < arr.len(), 
					        "out-of-bounds arr access: len is {}, index is {}", arr.len(), *self);

					Ok(index as usize)
				}
			}

			#[inline(always)]
			fn as_usize_excluded<A>(&self, arr: &A) -> GResult<usize> where A: DequeOps {
				if *self >= 0 {
					ensure!(*self as usize <= arr.len(), 
					        "out-of-bounds arr access: len is {}, index is {}", arr.len(), *self);
					Ok(*self as usize)
				} else {
					let index = arr.len() as isize + (*self as isize);
					ensure!(index >= 0 && index as usize <= arr.len(), 
					        "out-of-bounds arr access: len is {}, index is {}", arr.len(), *self);

					Ok(index as usize)
				}
			}
		}
	);
}

impl_arr_index_signed!(i8);
impl_arr_index_signed!(i16);
impl_arr_index_signed!(i32);
impl_arr_index_signed!(i64);
impl_arr_index_signed!(i128);
impl_arr_index_signed!(isize);

/**
An integer range type which can be used to slice a deque.

The [`DequeAccessRange`](trait.DequeAccessRange.html) trait is generic over this trait. This 
enables [`Arrs`](struct.Arr.html), [`Strs`](struct.Str.html) and [`Deques`](enum.Deque.html) to be
indexed using slices of any primitive integer type, including negative indexes.

This trait is [sealed]. It's not possible to implement this trait for your own types.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait DequeRange<I: DequeIndex>: RangeBounds<I> + deque_range_private::Sealed<I> {
	#[doc(hidden)]
	fn as_range<A>(&self, arr: &A) -> GResult<(Bound<usize>, Bound<usize>)> 
		where A: DequeOps;
}

mod deque_range_private {
	use std::ops::{RangeBounds};
	use super::{DequeIndex};

	pub trait Sealed<I> { }

	impl<T, I> Sealed<I> for T where T: RangeBounds<I>, I: DequeIndex { }
}

impl<T, I> DequeRange<I> for T where T: RangeBounds<I>, I: DequeIndex {
	fn as_range<A>(&self, arr: &A) -> GResult<(Bound<usize>, Bound<usize>)> 
	where 
		A: DequeOps 
	{
		let start_bound = match self.start_bound() {
			Bound::Included(index) => Bound::Included(index.as_usize(arr)?),
			Bound::Excluded(index) => {
				//this will never happen with the built-in range types. an "exclusive unsigned
				//start index into a vector" doesn't make much sense, so we just throw an error 
				//if it's less than zero.
				Bound::Excluded(index.as_usize(arr)?)
			}
			Bound::Unbounded => Bound::Unbounded
		};

		let end_bound = match self.end_bound() {
			Bound::Included(index) => Bound::Included(index.as_usize(arr)?),
			Bound::Excluded(index) => Bound::Excluded(index.as_usize_excluded(arr)?),
			Bound::Unbounded => Bound::Unbounded
		};

		Ok((start_bound, end_bound))
	}
}


//-------------------------------------------------------------------------------------------------
// DequeOps
//-------------------------------------------------------------------------------------------------

/**
A type which can be stored in a deque.

Types which implement `IntoElement<Slot>` can be stored in an [`Arr`](struct.Arr.html) or a
[`Deque`](enum.Deque.html). This has a blanket implementation for any type which implements 
[`ToVal`](trait.ToVal.html).

Types which implement `IntoElement<char>` can be stored in a [`Str`](struct.Str.html). This is
implemented for `char`, [`Val`](enum.Val.html), and shared and mutable references to those types.

This trait is [sealed]. It's not possible to implement this trait for your own types.
Implement [`ToVal`](trait.ToVal.html) instead.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait IntoElement<T>: into_element_private::Sealed<T> {
	fn into_item(self) -> GResult<T>;
}

mod into_element_private {
	use crate::{gc::Slot, val::Val, wrap::ToVal};
	
	pub trait Sealed<T> { }

	impl<T> Sealed<Slot> for T where T: ToVal { }
	impl Sealed<char> for char { }
	impl Sealed<char> for &char { }
	impl Sealed<char> for &mut char { }
	impl Sealed<char> for Val { }
	impl Sealed<char> for &Val { }
	impl Sealed<char> for &mut Val { }
	impl Sealed<char> for Slot { }
	impl Sealed<char> for &Slot { }
	impl Sealed<char> for &mut Slot { }
}

/**
A type which can be extracted from a deque.

Types which implement `FromElement<Slot>` can be taken out of an [`Arr`](struct.Arr.html) or a
[`Deque`](enum.Deque.html). This has a blanket implementation for any type which implements 
[`FromVal`](trait.FromVal.html).

Types which implement `FromElement<char>` can be taken out of a [`Str`](struct.Str.html). This is
implemented for `char` and [`Val`](enum.Val.html).

This trait is [sealed]. It's not possible to implement this trait for your own types.
Implement [`FromVal`](trait.FromVal.html) instead.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait FromElement<T>: Sized + from_element_private::Sealed<T> {
	fn from_item(item: &T) -> GResult<Self>;
}

mod from_element_private {
	use crate::{gc::Slot, val::Val, wrap::FromVal};

	pub trait Sealed<T> { }

	impl<T> Sealed<Slot> for T where T: FromVal { }
	impl Sealed<char> for char { }
	impl Sealed<char> for Val { }
	impl Sealed<char> for Slot { }
}

impl<T> IntoElement<Slot> for T where T: ToVal {
	#[inline(always)]
	fn into_item(self) -> GResult<Slot> {
		self.to_slot()
	}
}

impl<T> FromElement<Slot> for T where T: FromVal {
	#[inline(always)]
	fn from_item(item: &Slot) -> GResult<Self> {
		Self::from_slot(item)
	}
}

impl IntoElement<char> for char {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		Ok(self)
	}
}

impl IntoElement<char> for &char {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		Ok(*self)
	}
}

impl IntoElement<char> for &mut char {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		Ok(*self)
	}
}

impl IntoElement<char> for Val {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		match self {
			Val::Char(ch) => Ok(ch),
			_ => bail!("attempted to assign a {} to a str", self.type_name())
		}
	}
}

impl IntoElement<char> for &Val {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		match *self {
			Val::Char(ch) => Ok(ch),
			_ => bail!("attempted to assign a {} to a str", self.type_name())
		}
	}
}

impl IntoElement<char> for &mut Val {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		match *self {
			Val::Char(ch) => Ok(ch),
			_ => bail!("attempted to assign a {} to a str", self.type_name())
		}
	}
}

impl IntoElement<char> for Slot {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		match self {
			Slot::Char(ch) => Ok(ch),
			_ => bail!("attempted to assign a {} to a str", self.type_name())
		}
	}
}

impl IntoElement<char> for &Slot {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		match *self {
			Slot::Char(ch) => Ok(ch),
			_ => bail!("attempted to assign a {} to a str", self.type_name())
		}
	}
}

impl IntoElement<char> for &mut Slot {
	#[inline(always)]
	fn into_item(self) -> GResult<char> {
		match *self {
			Slot::Char(ch) => Ok(ch),
			_ => bail!("attempted to assign a {} to a str", self.type_name())
		}
	}
}

impl FromElement<char> for char {
	#[inline(always)]
	fn from_item(item: &char) -> GResult<Self> {
		Ok(*item)
	}
}

impl FromElement<char> for Val {
	#[inline(always)]
	fn from_item(item: &char) -> GResult<Self> {
		Ok(Val::Char(*item))
	}
}

impl FromElement<char> for Slot {
	#[inline(always)]
	fn from_item(item: &char) -> GResult<Self> {
		Ok(Slot::Char(*item))
	}
}

/**
The `deque` abstract type.

When manipulating an [`Arr`](struct.Arr.html), [`Str`](struct.Str.html) or 
[`Deque`](enum.Deque.html), you'll mostly use this trait's methods.

This trait defines all of a deque's operations other than indexing. For indexing a deque,
use the traits [`DequeAccess`](trait.DequeAccess.html) and 
[`DequeAccessRange`](trait.DequeAccessRange.html).

This trait is [sealed]. It's not possible to implement this trait for your own types.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait DequeOps: Sized + deque_ops_private::Sealed {
	type Element;
	type Item: FromElement<Self::Element>;

	/**
	Pushes an element to the end of the deque.
	
	Equivalent to [`(push! deq val)`](https://gamelisp.rs/std/push-mut).
	*/
	fn push<V: IntoElement<Self::Element>>(&self, val: V) -> GResult<()>;
	
	/**
	Pops an element from the end of the deque.
	
	Equivalent to [`(pop! deq)`](https://gamelisp.rs/std/pop-mut).
	*/
	fn pop<R: FromElement<Self::Element>>(&self) -> GResult<R>;
	
	/**
	Pushes an element to the beginning of the deque.
	
	Equivalent to [`(push-start! deq val)`](https://gamelisp.rs/std/push-start-mut).
	*/
	fn push_start<V: IntoElement<Self::Element>>(&self, val: V) -> GResult<()>;
	
	/**
	Pops an element from the beginning of the deque.
	
	Equivalent to [`(pop-start! deq)`](https://gamelisp.rs/std/pop-start-mut).
	*/
	fn pop_start<R: FromElement<Self::Element>>(&self) -> GResult<R>;
	
	/**
	Increases the deque's size.
	
	Equivalent to [`(grow! deq start-n end-n fill)`](https://gamelisp.rs/std/grow-mut).
	*/
	fn grow<V: IntoElement<Self::Element>>(
		&self,
		start_to_add: usize,
		end_to_add: usize,
		fill: V
	) -> GResult<()>;
	
	/**
	Decreases the deque's size.
	
	Equivalent to [`(shrink! deq start-n end-n`](https://gamelisp.rs/std/shrink-mut).
	*/
	fn shrink(&self, start_to_remove: usize, end_to_remove: usize) -> GResult<()>;
	
	///Pushes the contents of another deque onto the end of this one.
	fn append(&self, other: &Self) -> GResult<()>;

	///Pushes the contents of another deque onto the beginning of this one.
	fn prepend(&self, other: &Self) -> GResult<()>;

	/**
	Resizes the deque.

	Equivalent to [`VecDeque::resize`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.resize
	*/
	fn resize<V: IntoElement<Self::Element>>(&self, new_len: usize, val: V) -> GResult<()>;
	
	/**
	Rotates the deque to the left

	Equivalent to [`VecDeque::rotate_left`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.rotate_left
	*/
	fn rotate_left(&self, mid: usize) -> GResult<()>;
	
	/**
	Rotates the deque to the right.

	Equivalent to [`VecDeque::rotate_right`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.rotate_right
	*/
	fn rotate_right(&self, k: usize) -> GResult<()>;
	
	/**
	Swaps two of the deque's elements.

	Equivalent to [`(swap! [deq i] [deq j])`](https://gamelisp.rs/std/swap-mut).
	*/
	fn swap<I1: DequeIndex, I2: DequeIndex>(&self, i: I1, j: I2) -> GResult<()>;

	/**
	Returns the deque's storage capacity.

	Equivalent to [`VecDeque::capacity`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.capacity
	*/
	fn capacity(&self) -> usize;

	/**
	Returns the deque's length.

	Equivalent to [`(len deq)`](https://gamelisp.rs/std/len).
	*/
	fn len(&self) -> usize;

	/**
	Returns `true` if the deque's length is 0.

	Equivalent to [`(empty? deq)`](https://gamelisp.rs/std/empty-p).
	*/
	fn is_empty(&self) -> bool { self.len() == 0 }

	/**
	Reserves enough space for exactly `additional` elements to be added to the deque.

	Equivalent to [`VecDeque::reserve_exact`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.reserve_exact
	*/
	fn reserve_exact(&self, additional: usize) -> GResult<()>;
	
	/**
	Reserves enough space for at least `additional` elements to be added to the deque.

	Equivalent to [`VecDeque::reserve`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.reserve
	*/
	fn reserve(&self, additional: usize) -> GResult<()>;
	
	/**
	Shrinks the capacity of the deque as much as possible.

	Equivalent to [`VecDeque::shrink_to_fit`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.shrink_to_fit
	*/
	fn shrink_to_fit(&self) -> GResult<()>;
	
	/**
	Reduces the deque's length.

	Equivalent to [`VecDeque::truncate`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.truncate
	*/
	fn truncate(&self, len: usize) -> GResult<()>;

	/**
	Removes all of the deque's elements.

	Equivalent to [`VecDeque::clear`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.clear
	*/
	fn clear(&self) -> GResult<()>;

	/**
	Tests whether the deque contains an element which compares `eq?` to the argument.

	Equivalent to [`VecDeque::contains`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.contains
	*/
	fn contains<V: IntoElement<Self::Element>>(&self, x: V) -> GResult<bool>;

	/**
	Sorts the deque, in place. Elements are compared using [`Val::partial_cmp`][0]. 
	If the deque contains any two elements which are unordered relative to one another 
	(for example, an integer and a symbol), an error may occur.

	[0]: enum.Val.html#method.partial_cmp
	*/
	fn sort(&self) -> GResult<()>;

	/**
	Sorts the deque, in place, using the specified comparison function. If the comparison
	function returns an error, the `sort_by` call will return an error.
	*/
	fn sort_by<F>(&self, f: F) -> GResult<()>
		where F: FnMut(&Self::Item, &Self::Item) -> GResult<Ordering>;

	/**
	Makes the deque immutable.

	Equivalent to [`(freeze! deq)`](https://gamelisp.rs/std/freeze-mut).
	*/
	fn freeze(&self);

	/**
	Makes the deque and all of its contents immutable.

	Equivalent to [`(deep-freeze! deq)`](https://gamelisp.rs/std/deep-freeze-mut).
	*/
	fn deep_freeze(&self);

	///Returns `true` if the deque has been frozen.
	fn is_frozen(&self) -> bool;

	///Returns `true` if the deque and all of its contents have been frozen.
	fn is_deep_frozen(&self) -> bool;

	/**
	Returns `true` if it's possible to mutate the deque without triggering an error.
	
	This method will currently return `false` if the deque has been frozen, or if it's
	currently being iterated from Rust.
	*/
	fn can_mutate(&self) -> bool;

	//todo: starts_with, ends_with, sort_by. starts_with could be particularly useful for
	//pattern-matching: Val::Arr(ref arr) if arr.starts_with([SPLICE_SYM]) => { ... }.
	//possibly also set_slice, get_slice, remove_slice, insert_multi, push_multi and 
	//push_start_multi. those last two would make append and prepend redundant.

	//todo: if you keep append and prepend, they should accept T: DequeOps, not &Self

	#[doc(hidden)]
	fn lock(&self) -> Lock;

	/**
	Creates an infallible iterator over this deque.
	
	The iterator's item type will be [`Val`](enum.Val.html) for [`Arr`](struct.Arr.html) and
	[`Deque`](enum.Deque.html), or `char` for [`Str`](struct.Str.html).
	*/
	fn iter<'a>(&'a self) -> IterDeque<'a, Self>
	where
		Self: DequeAccess<usize>
	{
		IterDeque::from(self)
	}

	/**
	Creates a converting iterator over this deque.
	
	The iterator can produce any type which implements [`FromElement`](trait.FromElement.html),
	but each item will be wrapped in a [`GResult<T>`](type.GResult.html), so it will need
	to be unwrapped using `?` before it can be used.
	*/
	fn iter_to<'a, R: FromElement<Self::Element>>(&'a self) -> IterDequeTo<'a, Self, R> 
	where
		Self: DequeAccess<usize>
	{
		IterDequeTo::from(self)
	}

	//we can't implement the actual Extend trait, because it requires a &mut self receiver.

	/**
	Pushes the contents of an iterator onto the end of the deque.
	*/
	fn extend<I, V>(&self, source: I) -> GResult<()>
		where I: IntoIterator<Item = V>, V: IntoElement<Self::Element>;
}

mod deque_ops_private {
	use super::{Arr, Deque, Str};

	pub trait Sealed { }

	impl Sealed for Arr { }
	impl Sealed for Deque { }
	impl Sealed for Str { }
}

//we have to put get(), set() and so on in their own, generic traits because otherwise the 
//signatures would be like fn get<IndexType, ResultType>(...), which turbofishes as 
//arr.get::<_, i32>(0). there's precedent in the rust stdlib - Vecs implement Index<usize>, 
//Index<RangeFull>, etc.

/**
Indexing the `deque` abstract type.

When manipulating an [`Arr`](struct.Arr.html), [`Str`](struct.Str.html) or 
[`Deque`](enum.Deque.html), you'll mostly use this trait's methods, along with
[`DequeOps`](trait.DequeOps.html) and [`DequeAccessRange`](trait.DequeAccessRange.html).

This trait is [sealed]. It's not possible to implement this trait for your own types.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait DequeAccess<I: DequeIndex>: DequeOps {
	/**
	Accesses an element in the deque.

	Equivalent to [`[deq index]`](https://gamelisp.rs/std/access).
	*/
	fn get<R: FromElement<Self::Element>>(&self, index: I) -> GResult<R>;
	
	/**
	Mutates an element in the deque.

	Equivalent to [`(= [deq index] val)`](https://gamelisp.rs/std/set-access).
	*/
	fn set<V: IntoElement<Self::Element>>(&self, index: I, val: V) -> GResult<()>;
	
	/**
	Inserts an element into the deque at the given index.

	Equivalent to [`(insert! deq index val)`](https://gamelisp.rs/std/insert-mut).
	*/
	fn insert<V: IntoElement<Self::Element>>(&self, index: I, val: V) -> GResult<()>;
	
	/**
	Deletes an element from the deque, without returning it.

	Equivalent to [`(del! deq index)`](https://gamelisp.rs/std/del-mut).
	*/
	fn del(&self, index: I) -> GResult<()>;
	
	/**
	Removes an element from the deque and returns it.

	Equivalent to [`(remove! deq index)`](https://gamelisp.rs/std/remove-mut).
	*/
	fn remove<R: FromElement<Self::Element>>(&self, index: I) -> GResult<R>;
	
	/**
	Swaps an element with the last element, removes it, and returns it.

	Equivalent to [`(swap-remove! deq index)`](https://gamelisp.rs/std/swap-remove-mut).
	*/
	fn swap_remove<R: FromElement<Self::Element>>(&self, index: I) -> GResult<R>;
	
	/**
	Swaps an element with the first element, removes it, and returns it.

	Equivalent to [`(swap-remove-start! deq index)`][0].

	[0]: https://gamelisp.rs/std/swap-remove-start-mut
	*/
	fn swap_remove_start<R: FromElement<Self::Element>>(&self, index: I) -> GResult<R>;
}

/**
Indexing the `deque` abstract type.

When manipulating an [`Arr`](struct.Arr.html), [`Str`](struct.Str.html) or 
[`Deque`](enum.Deque.html), you'll mostly use this trait's methods, along with
[`DequeOps`](trait.DequeOps.html) and [`DequeAccess`](trait.DequeAccess.html).

This trait is [sealed]. It's not possible to implement this trait for your own types.

[sealed]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
*/

pub trait DequeAccessRange<I: DequeIndex, R: DequeRange<I> + Debug>: DequeAccess<I> {
	/**
	Removes multiple elements from the deque.

	Equivalent to [`(del! deq i : j)`](https://gamelisp.rs/std/del-mut).
	*/
	fn del_slice(&self, range: R) -> GResult<()>;
}

fn generic_grow<T: Clone>(
	vec: &mut VecDeque<T>,
	start_to_add: usize,
	end_to_add: usize,
	fill: T
) {
	vec.reserve(start_to_add + end_to_add);
	vec.extend(repeat(fill.clone()).take(end_to_add));
	for _ in 0 .. start_to_add {
		vec.push_front(fill.clone());
	}
}

fn generic_shrink<T>(
	vec: &mut VecDeque<T>,
	start_to_remove: usize, 
	end_to_remove: usize
) -> GResult<()> {
	ensure!(start_to_remove + end_to_remove <= vec.len(), 
	        "attempted to shrink a deque's len below 0");

	vec.truncate(vec.len() - end_to_remove);
	vec.drain(..start_to_remove);

	Ok(())
}


//-------------------------------------------------------------------------------------------------
// Arr
//-------------------------------------------------------------------------------------------------

/**
The `arr` primitive type.

Most of this type's methods belong to the `deque` abstract type, so they can be found in the 
traits [`DequeOps`](trait.DequeOps.html), [`DequeAccess`](trait.DequeAccess.html), and 
[`DequeAccessRange`](trait.DequeAccessRange.html).

New arrays can be constructed using the [`arr!` macro](macro.arr.html) or various toplevel
functions, such as [`glsp::arr`](fn.arr.html) and [`glsp::arr_from_iter`](fn.arr_from_iter.html).
Arrays are always stored on the garbage-collected heap, so they're normally represented by
the type [`Root<Arr>`](struct.Root.html).
*/

pub struct Arr {
	header: GcHeader,
	span: Cell<Span>,
	vec: RefCell<VecDeque<Slot>>
}

impl Allocate for Arr {
	fn header(&self) -> &GcHeader {
		&self.header
	}

	fn visit_gcs<V: Visitor>(&self, visitor: &mut V) {
		for slot in self.vec.borrow().iter() {
			visitor.visit_slot(slot);
		}
	}

	fn clear_gcs(&self) {
		self.vec.borrow_mut().clear()
	}

	fn owned_memory_usage(&self) -> usize {
		//todo: methods like this will panic if the arr is being iterated when glsp::gc() is called
		(self.vec.borrow().capacity() + 1) * size_of::<Slot>()
	}
}

impl Arr {
	pub(crate) fn new() -> Arr {
		Arr {
			header: GcHeader::new(), 
			span: Cell::new(Span::default()),
			vec: RefCell::new(VecDeque::new())
		}
	}

	pub(crate) fn with_capacity(capacity: usize) -> Arr {
		Arr {
			header: GcHeader::new(), 
			span: Cell::new(Span::default()),
			vec: RefCell::new(VecDeque::with_capacity(capacity))
		}
	}

	#[allow(dead_code)]
	pub(crate) fn from_elem<V: ToVal>(elem: V, reps: usize) -> GResult<Arr> {

		let mut vec = VecDeque::with_capacity(reps);
		for _ in 0 .. reps {
			//we deliberately call to_slot() multiple times, so that e.g. [(1, 2); 10] will
			//allocate ten distinct arrs, rather than ten aliases of the same arr.
			vec.push_back(elem.to_slot()?)
		}

		Ok(Arr {
			header: GcHeader::new(),
			span: Cell::new(Span::default()),
			vec: RefCell::new(vec)
		})
	}

	pub(crate) fn from_iter<T, V>(source: T) -> GResult<Arr> 
	where
		T: IntoIterator<Item = V>,
		V: ToVal
	{
		let iter = source.into_iter();
		let mut vec = VecDeque::with_capacity(iter.size_hint().0);
		for item in iter {
			vec.push_back(item.to_slot()?);
		}

		Ok(Arr {
			header: GcHeader::new(), 
			span: Cell::new(Span::default()),
			vec: RefCell::new(vec)
		})
	}

	/**
	Creates a shallow copy of an array.

	Equivalent to [`(clone ar)`](https://gamelisp.rs/std/clone).
	*/
	pub fn shallow_clone(&self) -> Root<Arr> {
		let arr = glsp::arr_from_iter(self.iter()).unwrap();
		arr.set_span(self.span());
		arr
	}

	/**
	Recursively copies an array and all of its contents.

	Equivalent to [`(deep-clone ar)`](https://gamelisp.rs/std/deep-clone).
	*/
	//todo: check for reference cycles
	pub fn deep_clone(&self) -> GResult<Root<Arr>> {
		let arr = glsp::arr_with_capacity(self.len());
		for val in self.iter() {
			arr.push(val.deep_clone()?).unwrap();
		}
		arr.set_span(self.span());
		Ok(arr)
	}

	/**
	Equivalent to [`(eq? self other)`](https://gamelisp.rs/std/eq-p).

	Note that, because this method may need to invoke an `op-eq?` method when one of its
	elements is an object or an `RData`, it can potentially fail.

	The same is true for `PartialEq` comparisons between arrays using Rust's `==` operator.
	In that case, if an error occurs, the operator will panic.
	*/
	pub fn try_eq(&self, other: &Arr) -> GResult<bool> {
		if self.len() != other.len() {
			return Ok(false)
		}

		for (v0, v1) in self.iter().zip(other.iter()) {
			if !v0.try_eq(&v1)? {
				return Ok(false)
			}
		}

		Ok(true)
	}

	/**
	Creates an indexing iterator for this collection.

	Equivalent to [`[ar iter]`](https://gamelisp.rs/std/access).
	*/
	pub fn access_giter(arr: &Root<Arr>, giter: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::AccessArr(arr.to_gc(), giter.to_gc()))
	}

	#[doc(hidden)]
	pub fn span(&self) -> Span {
		self.span.get()
	}

	#[doc(hidden)]
	pub fn set_span(&self, span: Span) {
		self.span.set(span)
	}

	pub(crate) fn borrow(&self) -> Ref<VecDeque<Slot>> {
		self.vec.borrow()
	}

	pub(crate) fn borrow_mut(&self) -> GResult<RefMut<VecDeque<Slot>>> {
		ensure!(!self.header.frozen(), "attempted to mutate a frozen arr");
		match self.vec.try_borrow_mut() {
			Ok(ref_mut) => Ok(ref_mut),
			Err(_) => bail!("attempted to mutate a borrowed arr")
		}
	}

	#[allow(dead_code)]
	fn write_barrier_val(&self, val: &Val) {
		with_heap(|heap| heap.write_barrier_val(self, val));
	}

	fn write_barrier_slot(&self, slot: &Slot) {
		with_heap(|heap| heap.write_barrier_slot(self, slot));
	}

	fn memory_usage_barrier(&self, prev_usage: usize, cur_usage: usize) {
		with_heap(|heap| heap.memory_usage_barrier(self, prev_usage, cur_usage));
	}

	fn borrow_mut_with_capacity_guard<R, F>(&self, f: F) -> GResult<R> 
	where
		F: FnOnce(&mut RefMut<VecDeque<Slot>>) -> GResult<R>
	{
		let prev_usage = self.owned_memory_usage();
		let _guard = Guard::new(|| {
			let cur_usage = self.owned_memory_usage();
			if prev_usage != cur_usage {
				self.memory_usage_barrier(prev_usage, cur_usage);
			}
		});

		let mut ref_mut = self.borrow_mut()?;

		f(&mut ref_mut)
	}
}

impl PartialEq<Arr> for Arr {
	fn eq(&self, other: &Arr) -> bool {
		self.try_eq(other).unwrap()
	}
}

impl Eq for Arr { }

impl PartialOrd<Arr> for Arr {
	fn partial_cmp(&self, other: &Arr) -> Option<Ordering> {
		self.iter().partial_cmp(other.iter())
	}
}

impl Hash for Arr {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.vec.borrow().hash(state)
	}
}

impl DequeOps for Arr {
	type Element = Slot;
	type Item = Val;

	fn freeze(&self) {
		self.header.freeze()
	}

	//todo: handle reference cycles
	fn deep_freeze(&self) {
		self.freeze();
		for element in self.iter() {
			element.deep_freeze();
		}
	}

	fn is_frozen(&self) -> bool {
		self.header.frozen()
	}

	//todo: handle reference cycles
	fn is_deep_frozen(&self) -> bool {
		self.header.frozen() && self.iter().all(|val| val.is_deep_frozen())
	}

	fn can_mutate(&self) -> bool {
		(!self.header.frozen()) && self.vec.try_borrow_mut().is_ok()
	}

	fn sort(&self) -> GResult<()> {
		self.sort_by(|v0, v1| {
			match v0.partial_cmp(v1) {
				Some(ordering) => Ok(ordering),
				None => bail!(
					"attempted to compare incompatible types: {} and {}",
					v0.a_type_name(),
					v1.a_type_name()
				)
			}
		})
	}

	fn sort_by<F>(&self, mut f: F) -> GResult<()>
	where
		F: FnMut(&Val, &Val) -> GResult<Ordering>
	{
		//rust's built-in sort_by() can only sort a slice
		let mut vec = SmallVec::<[Val; 64]>::from_iter(self.iter());

		//this is an ugly hack to provide error-propagation from within sort_by's callback. when an
		//error occurs, we store it on the stack, repeatedly return Ordering::Equal until sort_by
		//returns (on the assumption that this (1) won't trigger any endless loops, and (2) will
		//enable sort_by to return as early as possible), and then return the error.
		let mut error: Option<GError> = None;
		vec.sort_by(|a, b| {
			if error.is_some() {
				return Ordering::Equal
			}

			match f(a, b) {
				Ok(ordering) => ordering,
				Err(err) => {
					error = Some(err);
					Ordering::Equal
				}
			}
		});

		//copy the sorted values back into our storage
		match error {
			Some(error) => Err(error),
			None => {
				for (i, val) in vec.drain(..).enumerate() {
					self.set(i, val).unwrap();
				}

				Ok(())
			}
		}
	}

	fn push<V: IntoElement<Slot>>(&self, val: V) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|vec| {
			let val = val.into_item()?;
			self.write_barrier_slot(&val);
			vec.push_back(val);
			Ok(())
		})
	}

	//methods which just delete elements (rather than updating or adding them) don't need to be
	//write-barriered, since they cannot create new references into the gc heap.
	fn pop<R: FromElement<Slot>>(&self) -> GResult<R> {
		match self.borrow_mut()?.pop_back() {
			Some(val) => R::from_item(&val),
			None => bail!("attempted to pop from an arr of length 0")
		}
	}

	fn push_start<V: IntoElement<Slot>>(&self, val: V) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|vec| {
			let val = val.into_item()?;
			self.write_barrier_slot(&val);
			vec.push_front(val);
			Ok(())
		})
	}

	fn pop_start<R: FromElement<Slot>>(&self) -> GResult<R> {
		match self.borrow_mut()?.pop_front() {
			Some(val) => R::from_item(&val),
			None => bail!("attempted to pop from the start of an arr of length 0")
		}
	}

	fn grow<V: IntoElement<Slot>>(&self, start_to_add: usize, 
	                          end_to_add: usize, fill: V) -> GResult<()> {
		if start_to_add + end_to_add > 0 {
			self.borrow_mut_with_capacity_guard(|vec| {
				let fill = fill.into_item()?;
				self.write_barrier_slot(&fill);
				generic_grow(vec, start_to_add, end_to_add, fill);
				Ok(())
			})
		} else {
			Ok(())
		}
	}

	fn shrink(&self, start_to_remove: usize, end_to_remove: usize) -> GResult<()> {
		generic_shrink(&mut *(self.borrow_mut()?), start_to_remove, end_to_remove)
	}

	fn append(&self, other: &Arr) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|self_vec| {
			let other_vec = other.borrow();

			self_vec.reserve(other_vec.len());

			for slot in other_vec.iter() {
				self_vec.push_back(slot.clone());
				self.write_barrier_slot(slot);
			}

			Ok(())
		})
	}

	fn prepend(&self, other: &Arr) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|self_vec| {
			let other_vec = other.borrow();

			self_vec.reserve(other_vec.len());

			for slot in other_vec.iter().rev() {
				self_vec.push_front(slot.clone());
				self.write_barrier_slot(slot);
			}

			Ok(())
		})
	}

	fn resize<V: IntoElement<Slot>>(&self, new_len: usize, fill: V) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|self_vec| {
			let fill = fill.into_item()?;

			let old_len = self_vec.len();
			if new_len > old_len {
				self.write_barrier_slot(&fill);
			}

			self_vec.resize(new_len, fill);			

			Ok(())
		})
	}

	fn rotate_left(&self, mid: usize) -> GResult<()> {
		self.borrow_mut()?.rotate_left(mid);
		Ok(())
	}

	fn rotate_right(&self, k: usize) -> GResult<()> {
		self.borrow_mut()?.rotate_right(k);
		Ok(())
	}

	fn swap<I1, I2>(&self, index1: I1, index2: I2) -> GResult<()> 
	where 
		I1: DequeIndex, 
		I2: DequeIndex 
	{
		let i1 = index1.as_usize(self)?;
		let i2 = index2.as_usize(self)?;
		self.borrow_mut()?.swap(i1, i2);
		Ok(())
	}

	fn capacity(&self) -> usize {
		self.borrow().capacity()
	}

	fn len(&self) -> usize {
		self.borrow().len()
	}

	fn reserve_exact(&self, additional: usize) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|vec| {
			vec.reserve_exact(additional);
			Ok(())
		})
	}

	fn reserve(&self, additional: usize) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|vec| {
			vec.reserve(additional);
			Ok(())
		})
	}

	fn shrink_to_fit(&self) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|vec| {
			vec.shrink_to_fit();
			Ok(())
		})
	}

	fn truncate(&self, len: usize) -> GResult<()> {
		self.borrow_mut()?.truncate(len);
		Ok(())
	}

	fn clear(&self) -> GResult<()> {
		self.borrow_mut()?.clear();
		Ok(())
	}

	fn contains<V: IntoElement<Slot>>(&self, x: V) -> GResult<bool> {
		Ok(self.borrow().contains(&x.into_item()?))
	}

	fn extend<I, V>(&self, source: I) -> GResult<()>
	where 
		I: IntoIterator<Item = V>, 
		V: IntoElement<Slot>
	{
		//we don't leave a borrow_mut() outstanding while executing user code, because it may
		//do something weird which needs immutable access to the Arr, e.g. a garbage collection.
		let iter = source.into_iter();

		self.borrow_mut_with_capacity_guard(|vec| {
			vec.reserve(iter.size_hint().0);
			Ok(())
		})?;

		for val in iter {
			let val = val.into_item()?;

			self.write_barrier_slot(&val);
			self.borrow_mut_with_capacity_guard(|vec| {
				vec.push_back(val);
				Ok(())
			})?;
		}

		Ok(())
    }

	#[doc(hidden)]
    fn lock(&self) -> Lock {
    	Lock::Arr(self.borrow())
    }
}

impl<I: DequeIndex> DequeAccess<I> for Arr {
	fn get<R: FromElement<Slot>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		R::from_item(&self.borrow()[i])
	}

	fn set<V: IntoElement<Slot>>(&self, index: I, val: V) -> GResult<()> {
		let i = index.as_usize(self)?;

		let val = val.into_item()?;
		self.write_barrier_slot(&val);
		self.borrow_mut()?[i] = val;
		Ok(())
	}

	fn insert<V: IntoElement<Slot>>(&self, index: I, val: V) -> GResult<()> {
		let i = index.as_usize_excluded(self)?;

		self.borrow_mut_with_capacity_guard(|vec| {
			let val = val.into_item()?;
			self.write_barrier_slot(&val);
			vec.insert(i, val);
			Ok(())
		})
	}

	fn del(&self, index: I) -> GResult<()> {
		let i = index.as_usize(self)?;
		self.borrow_mut()?.remove(i).unwrap();
		Ok(())
	}

	fn remove<R: FromElement<Slot>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		let slot = self.borrow_mut()?.remove(i).unwrap();
		R::from_item(&slot)
	}

	fn swap_remove<R: FromElement<Slot>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		let slot = self.borrow_mut()?.swap_remove_back(i).unwrap();
		R::from_item(&slot)
	}

	fn swap_remove_start<R: FromElement<Slot>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		let slot = self.borrow_mut()?.swap_remove_front(i).unwrap();
		R::from_item(&slot)
	}
}

impl<I: DequeIndex, R: DequeRange<I> + Debug> DequeAccessRange<I, R> for Arr {
	fn del_slice(&self, range: R) -> GResult<()> {
		let r = range.as_range(self)?;
		self.borrow_mut()?.drain(r);
		Ok(())
	}
}


//-------------------------------------------------------------------------------------------------
// arr!, try_arr!, Splay
//-------------------------------------------------------------------------------------------------

/**
A type which can be passed to the [`arr!` macro](macro.arr.html) as a splayed argument.
*/ 

//we take a reference to the splayed argument in arr![] and try_arr![]. this means that even if 
//the user passes in a by-value [T; n], Root<T>, Rc<T>, etc., autoderef should still kick in.
pub trait Splay {
	fn splay(&self, dst: &Arr) -> GResult<()>;
}

impl Splay for Arr {
	fn splay(&self, dst: &Arr) -> GResult<()> {
		dst.extend(self.iter())
	}
}

impl Splay for Str {
	fn splay(&self, dst: &Arr) -> GResult<()> {
		dst.extend(self.iter())
	}
}

impl Splay for Deque {
	fn splay(&self, dst: &Arr) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.splay(dst),
			Deque::Str(st) => st.splay(dst)
		}
	}
}

impl Splay for Val {
	fn splay(&self, dst: &Arr) -> GResult<()> {
		match *self {
			Val::Arr(ref ar) => ar.splay(dst),
			Val::Str(ref st) => st.splay(dst),
			_ => bail!("attempted to splay {}", self.a_type_name())
		}
	}
}

impl<T> Splay for [T] where T: ToVal {
	fn splay(&self, dst: &Arr) -> GResult<()> {
		dst.reserve(self.len())?;
		for item in self {
			dst.push(item)?;
		}
		Ok(())
	}
}

//unlike Vec<T> etc., VecDeque<T> can't be dereferenced to &[T], so they need a separate impl
impl<T> Splay for VecDeque<T> where T: ToVal {
	fn splay(&self, dst: &Arr) -> GResult<()> {
		dst.reserve(self.len())?;
		for item in self {
			dst.push(item)?;
		}
		Ok(())
	}
}

macro_rules! impl_splay_tuple {
	($len:literal: $($t:ident $i:tt),+) => (
		impl<$($t),+> Splay for ($($t,)+) 
		where 
			$( for<'a> &'a $t: ToVal ),+ 
		{
			fn splay(&self, dst: &Arr) -> GResult<()> {
				dst.reserve($len)?;

				$(
					dst.push(&(self.$i))?;
				)+

				Ok(())
			}
		}
	);
}

impl_splay_tuple!( 1: A 0);
impl_splay_tuple!( 2: A 0, B 1);
impl_splay_tuple!( 3: A 0, B 1, C 2);
impl_splay_tuple!( 4: A 0, B 1, C 2, D 3);
impl_splay_tuple!( 5: A 0, B 1, C 2, D 3, E 4);
impl_splay_tuple!( 6: A 0, B 1, C 2, D 3, E 4, F 5);
impl_splay_tuple!( 7: A 0, B 1, C 2, D 3, E 4, F 5, G 6);
impl_splay_tuple!( 8: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
impl_splay_tuple!( 9: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
impl_splay_tuple!(10: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);
impl_splay_tuple!(11: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10);
impl_splay_tuple!(12: A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10, L 11);

#[doc(hidden)]
#[macro_export]
macro_rules! try_arr_capacity {
	(.. $splayed:expr) => (1);
	($item:expr) => (1);
	(.. $splayed:expr, $($token:tt)*) => (
		1 + $crate::try_arr_capacity!($($token)*)
	);
	($item:expr, $($token:tt)*) => (
		1 + $crate::try_arr_capacity!($($token)*)
	);
}

#[doc(hidden)]
#[macro_export]
macro_rules! try_arr_clauses {
	($arr:ident, .. $splayed:expr) => ({
		use $crate::Splay;
		match (&$splayed).splay(&$arr) {
			Ok(()) => (),
			Err(err) => break $crate::GResult::Err(err)
		}
	});
	($arr:ident, $item:expr) => ({
		match $arr.push($item) {
			Ok(()) => (),
			Err(err) => break $crate::GResult::Err(err)
		}
	});
	($arr:ident, .. $splayed:expr, $($token:tt)*) => ({
		$crate::try_arr_clauses!($arr, ..$splayed);
		$crate::try_arr_clauses!($arr, $($token)*);
	});
	($arr:ident, $item:expr, $($token:tt)*) => ({
		$crate::try_arr_clauses!($arr, $item);
		$crate::try_arr_clauses!($arr, $($token)*);
	});
}

/**
A non-panicking version of [`arr!`](macro.arr.html).

Returns a [`GResult<Root<Arr>>`](struct.Arr.html).
*/

#[macro_export]
macro_rules! try_arr {
	($elem:expr; $n:expr) => (
		$crate::arr_from_elem($elem, $n)
	);
	() => (
		$crate::GResult::Ok($crate::arr())
	);
	($($token:tt)*) => (
        {
        	//we only use this loop so that we can break with a value (`try` blocks are nyi)
        	loop {
        		let arr = $crate::arr_with_capacity(
        			$crate::try_arr_capacity!($($token)*)
        		);
        		$crate::try_arr_clauses!(arr, $($token)*);
	        	break $crate::GResult::Ok(arr)
	        }
        }
    );
}

/**
Constructs an array.

The syntax is similar to the [`vec!` macro][0]. `arr![elem; n]` will repeat an element `n` times,
or `arr![a, b, c]` will store several individual elements. The return type is
[`Root<Arr>`](struct.Arr.html).

[0]: https://doc.rust-lang.org/std/macro.vec.html

Splaying is supported for types which implement the [`Splay` trait](trait.Splay.html). The syntax
is identical to GameLisp: `arr![a, ..b, c]`.

In the unlikely event that an argument fails to be converted to a [`Val`](enum.val.html), the 
macro will panic. [`try_arr!`](macro.try_arr.html) is the non-panicking alternative.
*/

#[macro_export]
macro_rules! arr {
	($elem:expr; $n:expr) => ($crate::try_arr![$elem; $n].unwrap());
	($($token:tt)*) => ($crate::try_arr![$($token)*].unwrap());
}


//-------------------------------------------------------------------------------------------------
// StrStorage, CharStorage
//-------------------------------------------------------------------------------------------------

#[doc(hidden)]
#[derive(Clone)]
pub enum StrStorage {
	Str1(VecDeque<CharStorage<u8>>),
	Str2(VecDeque<CharStorage<u16>>),
	Str4(VecDeque<CharStorage<char>>)
}

impl StrStorage {
	#[allow(dead_code)]
	pub(crate) fn len(&self) -> usize {
		match *self {
			StrStorage::Str1(ref vec) => vec.len(),
			StrStorage::Str2(ref vec) => vec.len(),
			StrStorage::Str4(ref vec) => vec.len()
		}
	}

	#[allow(dead_code)]
	pub(crate) fn chars_eq(&self, other: &StrStorage) -> bool {
		if self.len() != other.len() {
			false
		} else {
			for i in 0 .. self.len() {
				let self_ch = match *self {
					StrStorage::Str1(ref vec) => vec[i].into_char(),
					StrStorage::Str2(ref vec) => vec[i].into_char(),
					StrStorage::Str4(ref vec) => vec[i].into_char()
				};

				let other_ch = match *other {
					StrStorage::Str1(ref vec) => vec[i].into_char(),
					StrStorage::Str2(ref vec) => vec[i].into_char(),
					StrStorage::Str4(ref vec) => vec[i].into_char()
				};

				if self_ch != other_ch {
					return false
				}
			}

			true
		}
	}

	fn widen(&mut self, new_width: usize) {
		use StrStorage::*;

		let new_self = match (self as &StrStorage, new_width) {
			(&Str1(ref vec), 2) => {
				Str2(VecDeque::from_iter(vec.iter().map(|ch| ch.widen())))
			}
			(&Str1(ref vec), 4) => {
				Str4(VecDeque::from_iter(vec.iter().map(|ch| ch.widen())))
			}
			(&Str2(ref vec), 4) => {
				Str4(VecDeque::from_iter(vec.iter().map(|ch| ch.widen())))
			}
			_ => panic!()
		};

		*self = new_self;
	}

	pub(crate) fn prepare_for_char(&mut self, ch: char) {
		match (self as &StrStorage, ch as u32) {
			(&StrStorage::Str1(_), ch) if ch > u16::MAX as u32 => self.widen(4),
			(&StrStorage::Str1(_), ch) if ch > u8::MAX as u32 => self.widen(2),
			(&StrStorage::Str2(_), ch) if ch > u16::MAX as u32 => self.widen(4),
			_ => ()
		}
	}

	fn prepare_for_chars(&mut self, other: &StrStorage) {
		match (self as &StrStorage, other) {
			(&StrStorage::Str1(_), StrStorage::Str4(_)) => self.widen(4),
			(&StrStorage::Str1(_), StrStorage::Str2(_)) => self.widen(2),
			(&StrStorage::Str2(_), StrStorage::Str4(_)) => self.widen(4),
			_ => ()
		}
	}

	pub(crate) fn append(&mut self, other: &StrStorage) {
		self.prepare_for_chars(other);
		match (self, other) {
			(StrStorage::Str1(ref mut self_vec), StrStorage::Str1(ref other_vec)) => {
				self_vec.extend(other_vec.iter().copied())
			}
			(StrStorage::Str2(ref mut self_vec), StrStorage::Str1(ref other_vec)) => {
				for char_storage in other_vec.iter() {
					self_vec.push_back(char_storage.widen())
				}
			}
			(StrStorage::Str2(ref mut self_vec), StrStorage::Str2(ref other_vec)) => {
				self_vec.extend(other_vec.iter().copied())
			}
			(StrStorage::Str4(ref mut self_vec), StrStorage::Str1(ref other_vec)) => {
				for char_storage in other_vec.iter() {
					self_vec.push_back(char_storage.widen())
				}
			}
			(StrStorage::Str4(ref mut self_vec), StrStorage::Str2(ref other_vec)) => {
				for char_storage in other_vec.iter() {
					self_vec.push_back(char_storage.widen())
				}
			}
			(StrStorage::Str4(ref mut self_vec), StrStorage::Str4(ref other_vec)) => {
				self_vec.extend(other_vec.iter().copied())
			}
			_ => unreachable!()
		}
	}

	pub(crate) fn prepend(&mut self, other: &StrStorage) {
		self.prepare_for_chars(other);
		match (self, other) {
			(StrStorage::Str1(ref mut self_vec), StrStorage::Str1(ref other_vec)) => {
				for char_storage in other_vec.iter().rev() {
					self_vec.push_front(*char_storage)
				}
			}
			(StrStorage::Str2(ref mut self_vec), StrStorage::Str1(ref other_vec)) => {
				for char_storage in other_vec.iter().rev() {
					self_vec.push_front(char_storage.widen())
				}
			}
			(StrStorage::Str2(ref mut self_vec), StrStorage::Str2(ref other_vec)) => {
				for char_storage in other_vec.iter().rev() {
					self_vec.push_front(*char_storage)
				}
			}
			(StrStorage::Str4(ref mut self_vec), StrStorage::Str1(ref other_vec)) => {
				for char_storage in other_vec.iter().rev() {
					self_vec.push_front(char_storage.widen())
				}
			}
			(StrStorage::Str4(ref mut self_vec), StrStorage::Str2(ref other_vec)) => {
				for char_storage in other_vec.iter().rev() {
					self_vec.push_front(char_storage.widen())
				}
			}
			(StrStorage::Str4(ref mut self_vec), StrStorage::Str4(ref other_vec)) => {
				for char_storage in other_vec.iter().rev() {
					self_vec.push_front(*char_storage)
				}
			}
			_ => unreachable!()
		}
	}
}

//evaluates storage_expr, which must have the type &StrStorage. pattern-matches on the StrStorage, 
//binding a reference to the storage vec to vec_name, and executing body_expr for each variant. 
//each ch_name must be bound to a local variable of type &char, which is converted to an 
//appropriately-sized CharStorage and bound to a fresh local variable with the same name.
macro_rules! with_str_storage {
	($storage_expr:expr, $vec_name:ident, ($($ch_name:ident),*), $body_expr:expr) => (
		{
			match $storage_expr {
				StrStorage::Str1(ref $vec_name) => {
					$(let $ch_name = CharStorage::<u8>::from_char(*$ch_name);)*
					$body_expr
				}
				StrStorage::Str2(ref $vec_name) => {
					$(let $ch_name = CharStorage::<u16>::from_char(*$ch_name);)*
					$body_expr
				}
				StrStorage::Str4(ref $vec_name) => {
					$(let $ch_name = CharStorage::<char>::from_char(*$ch_name);)*
					$body_expr
				}
			}
		}
	);
}

//similar to with_str_storage. storage_expr must have type &mut StrStorage. calls prepare_for_char 
//for each ch_name.
macro_rules! with_str_storage_mut {
	($storage_expr:expr, $vec_name:ident, ($($ch_name:ident),*), $body_expr:expr) => (
		{
			let storage_ref: &mut StrStorage = $storage_expr;
			$(storage_ref.prepare_for_char(*$ch_name);)*
			match storage_ref {
				StrStorage::Str1(ref mut $vec_name) => {
					$(let $ch_name = CharStorage::<u8>::from_char(*$ch_name);)*
					$body_expr
				}
				StrStorage::Str2(ref mut $vec_name) => {
					$(let $ch_name = CharStorage::<u16>::from_char(*$ch_name);)*
					$body_expr
				}
				StrStorage::Str4(ref mut $vec_name) => {
					$(let $ch_name = CharStorage::<char>::from_char(*$ch_name);)*
					$body_expr
				}
			}
		}
	);
}

#[doc(hidden)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CharStorage<T: CoerceFromChar + Into<u32> + Copy + Eq + Ord>(T);

impl<T: CoerceFromChar + Into<u32> + Copy + Eq + Ord> CharStorage<T> {
	//there's a potential performance cost here, but there's no way to perform it as a "free"
	//coercion, because not all values of u16 or u32 are valid values for char. once we have
	//specialisation (todo), we will be able to specialise for the "free" u8-to-char coercion.
	pub(crate) fn into_char(&self) -> char {
		let u: u32 = self.0.into();
		char::try_from(u).unwrap()
	}

	//if the destination type is too narrow, the value will be truncated. it's the caller's
	//responsibility to make sure the StrStorage is wide enough for any incoming data!
	pub(crate) fn from_char(ch: char) -> Self {
		CharStorage(T::coerce_from_char(ch))
	}
}

impl<T: Hash + Copy + CoerceFromChar + Into<u32> + Eq + Ord> Hash for CharStorage<T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.0.hash(state)
	}
}

#[doc(hidden)]
pub trait CoerceFromChar: Sized {
	fn coerce_from_char(arg: char) -> Self;
}

impl CoerceFromChar for u8 {
	fn coerce_from_char(arg: char) -> u8 {
		arg as u32 as u8
	}
}

impl CoerceFromChar for u16 {
	fn coerce_from_char(arg: char) -> u16 {
		arg as u32 as u16
	}
}

impl CoerceFromChar for char {
	fn coerce_from_char(arg: char) -> char {
		arg
	}
}

pub(crate) trait Widen<T> {
	fn widen(self) -> T;
}

impl Widen<CharStorage<u16>> for CharStorage<u8> {
	fn widen(self) -> CharStorage<u16> {
		CharStorage(self.0 as u16)
	}
}

impl Widen<CharStorage<char>> for CharStorage<u8> {
	fn widen(self) -> CharStorage<char> {
		CharStorage(self.0 as char)
	}
}

impl Widen<CharStorage<char>> for CharStorage<u16> {
	fn widen(self) -> CharStorage<char> {
		CharStorage(char::try_from(self.0 as u32).unwrap())
	}
}


//-------------------------------------------------------------------------------------------------
// Str
//-------------------------------------------------------------------------------------------------

/**
The `str` primitive type.

Most of this type's methods belong to the `deque` abstract type, so they can be found in the 
traits [`DequeOps`](trait.DequeOps.html), [`DequeAccess`](trait.DequeAccess.html), and 
[`DequeAccessRange`](trait.DequeAccessRange.html).

New strings can be constructed using the [`str!` macro](macro.str.html) or various toplevel
functions, such as [`glsp::str`](fn.str.html) and [`glsp::str_from_iter`](fn.str_from_iter.html).
Strings are always stored on the garbage-collected heap, so they're normally represented by
the type [`Root<Str>`](struct.Root.html).
*/

pub struct Str {
	header: GcHeader,
	storage: RefCell<StrStorage>
}

impl Allocate for Str {
	fn header(&self) -> &GcHeader {
		&self.header
	}

	fn visit_gcs<V: Visitor>(&self, _visitor: &mut V) {
		//deliberate no-op
	}

	fn clear_gcs(&self) {
		//deliberate no-op
	}

	fn owned_memory_usage(&self) -> usize {
		match *self.borrow() {
			StrStorage::Str1(ref vec) => (vec.capacity() + 1),
			StrStorage::Str2(ref vec) => (vec.capacity() + 1) * 2,
			StrStorage::Str4(ref vec) => (vec.capacity() + 1) * 4
		}
	}
}

/**
Constructs a string.

The syntax is similar to the [`format!` macro][0]. `str!("hello, {}", x)` will return a
newly-allocated `Root<Str>`, with the text representation of `x` printed into it.

[0]: https://doc.rust-lang.org/std/macro.format.html
*/

#[macro_export]
macro_rules! str {
	($($arg:tt)*) => (
		{
			use std::fmt::Write;
			let mut st = glsp::str();
			write!(st, $($arg)*).unwrap();
			st
		}
	);
}

impl Str {
	pub(crate) fn new() -> Str {
		Str {
			header: GcHeader::new(),
			storage: RefCell::new(StrStorage::Str1(VecDeque::new()))
		}
	}

	pub(crate) fn from_rust_str(st: &str) -> Str {
		let max_char = st.chars().max().unwrap_or(0 as char);
		let storage = match max_char as u32 {
			0 ..= 0xff => {
				StrStorage::Str1(VecDeque::from_iter(st.chars().map(|ch| {
					CharStorage::<u8>::from_char(ch)
				})))
			}
			0x100 ..= 0xffff => {
				StrStorage::Str2(VecDeque::from_iter(st.chars().map(|ch| {
					CharStorage::<u16>::from_char(ch)
				})))
			}
			_ => {
				StrStorage::Str4(VecDeque::from_iter(st.chars().map(|ch| {
					CharStorage::<char>::from_char(ch)
				})))
			}
		};

		Str {
			header: GcHeader::new(),
			storage: RefCell::new(storage)
		}
	}

	pub(crate) fn from_iter<T>(iter: T) -> GResult<Str> 
	where
		T: IntoIterator,
		T::Item: IntoElement<char>
	{
		use StrStorage::*;

		let mut storage = Str1(VecDeque::new());
		for item in iter.into_iter() {
			let ch = item.into_item()?;
			storage.prepare_for_char(ch);
			match storage {
				Str1(ref mut vec) => vec.push_back(CharStorage::<u8>::from_char(ch)),
				Str2(ref mut vec) => vec.push_back(CharStorage::<u16>::from_char(ch)),
				Str4(ref mut vec) => vec.push_back(CharStorage::<char>::from_char(ch))
			}
		}

		Ok(Str {
			header: GcHeader::new(),
			storage: RefCell::new(storage)
		})
	}

	pub(crate) fn with_capacity(capacity: usize) -> Str {
		Str {
			header: GcHeader::new(),
			storage: RefCell::new(StrStorage::Str1(VecDeque::with_capacity(capacity)))
		}
	}

	pub(crate) fn to_rust_string(&self) -> String {
		use StrStorage::*;
		match *self.borrow() {
			Str1(ref vec) => String::from_iter(vec.iter().map(|&ch| ch.into_char())),
			Str2(ref vec) => String::from_iter(vec.iter().map(|&ch| ch.into_char())),
			Str4(ref vec) => String::from_iter(vec.iter().map(|&ch| ch.into_char()))
		}
	}

	pub(crate) fn to_escaped_string(&self) -> String {
		with_str_storage!(&*self.borrow(), vec, (), {
			let mut builder = String::new();

			for ch in vec.iter().map(|item| item.into_char()) {
				match ch {
					'\n' => builder.push_str("\\n"),
					'\t' => builder.push_str("\\t"),
					'\r' => builder.push_str("\\r"),
					'\\' => builder.push_str("\\\\"),
					'"' => builder.push_str("\\\""), 
					'\0' => builder.push_str("\\0"),
					'{' => builder.push_str("{{"),
					'}' => builder.push_str("}}"),
					ch => {
						if (ch as u32) < 32 {
							//these are all boring non-displayable control codes other than \n,
							//\r, \t and \0, which have already been handled above
							builder.push_str("\\x");
							builder.push(char::from_digit((ch as u32) / 16, 2).unwrap());
							builder.push(char::from_digit((ch as u32) & 0xf, 16).unwrap());
						} else {
							builder.push(ch)
						}
					}
				}
			}

			builder
		})
	}

	/**
	Creates a new string with a copy of this string's contents.

	Equivalent to [`(clone st)`](https://gamelisp.rs/std/clone).
	*/
	pub fn shallow_clone(&self) -> Root<Str> {
		glsp::str_from_iter(self.iter()).unwrap()
	}

	/**
	Creates an indexing iterator for this collection.

	Equivalent to [`[st iter]`](https://gamelisp.rs/std/access).
	*/
	pub fn access_giter(st: &Root<Str>, giter: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::AccessStr(st.to_gc(), giter.to_gc()))
	}

	///Pushes all of the characters from a `&str` to the end of this string.
	pub fn push_rust_str(&self, s: &str) -> GResult<()> {
		let mut max_ch = '\0';
		let mut ch_count = 0;
		for ch in s.chars() {
			if ch > max_ch {
				max_ch = ch;
			}
			ch_count += 1;
		}

		self.borrow_mut_with_capacity_guard(|storage| {
			storage.prepare_for_char(max_ch);
			with_str_storage_mut!(storage, vec, (), {
				vec.reserve(ch_count);
				for ch in s.chars() {
					vec.push_back(CharStorage::from_char(ch));
				}
			});

			Ok(())
		})
	}

	fn borrow(&self) -> Ref<StrStorage> {
		self.storage.borrow()
	}

	fn borrow_mut(&self) -> GResult<RefMut<StrStorage>> {
		ensure!(!self.header.frozen(), "attempted to mutate a frozen str");
		match self.storage.try_borrow_mut() {
			Ok(ref_mut) => Ok(ref_mut),
			Err(_) => bail!("attempted to mutate a borrowed str")
		}
	}

	fn memory_usage_barrier(&self, prev_usage: usize, cur_usage: usize) {
		with_heap(|heap| heap.memory_usage_barrier(self, prev_usage, cur_usage));
	}

	fn borrow_mut_with_capacity_guard<R, F>(&self, f: F) -> GResult<R> 
	where
		F: FnOnce(&mut RefMut<StrStorage>) -> GResult<R>
	{
		let prev_usage = self.owned_memory_usage();
		let _guard = Guard::new(|| {
			let cur_usage = self.owned_memory_usage();
			if prev_usage != cur_usage {
				self.memory_usage_barrier(prev_usage, cur_usage);
			}
		});

		let mut ref_mut = self.borrow_mut()?;

		f(&mut ref_mut)
	}
}

//if we were to implement fmt::Write for Str directly, it would require a &mut Str receiver, and
//it would therefore be impossible to call. the compromise is to implement it for Root<Str>.
//unfortunately, even that requires the Root<Str> to be declared as `mut` unnecessarily.

impl fmt::Write for Root<Str> {
	fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
		match self.push_rust_str(s) {
			Ok(()) => Ok(()),
			Err(_) => Err(fmt::Error::default())
		}
	}
}

impl PartialEq<Str> for Str {
	fn eq(&self, other: &Str) -> bool {
		self.iter().eq(other.iter())
	}
}

impl Eq for Str { }

impl PartialOrd<Str> for Str {
	fn partial_cmp(&self, other: &Str) -> Option<Ordering> {
		Some(self.iter().cmp(other.iter()))
	}
}

impl Ord for Str {
	fn cmp(&self, other: &Str) -> Ordering {
		self.iter().cmp(other.iter())
	}
}

impl Hash for Str {
	fn hash<H: Hasher>(&self, state: &mut H) {
		match *self.borrow() {
			StrStorage::Str1(ref vec) => vec.hash(state),
			StrStorage::Str2(ref vec) => vec.hash(state),
			StrStorage::Str4(ref vec) => vec.hash(state)
		}
	}
}

impl DequeOps for Str {
	type Element = char;
	type Item = char;

	fn freeze(&self) {
		self.header.freeze()
	}

	fn deep_freeze(&self) {
		self.freeze();
	}

	fn is_frozen(&self) -> bool {
		self.header.frozen()
	}

	fn is_deep_frozen(&self) -> bool {
		self.is_frozen()
	}

	fn can_mutate(&self) -> bool {
		(!self.header.frozen()) && self.storage.try_borrow_mut().is_ok()
	}

	fn sort(&self) -> GResult<()> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.make_contiguous().sort();
			Ok(())
		})
	}

	fn sort_by<F>(&self, mut f: F) -> GResult<()>
	where
		F: FnMut(&char, &char) -> GResult<Ordering>
	{
		//we don't want to borrow the Str while executing an arbitrary user callback
		let mut vec = SmallVec::<[char; 128]>::from_iter(self.iter());

		//this is an ugly hack to provide error-propagation from within sort_by's callback. when an
		//error occurs, we store it on the stack, repeatedly return Ordering::Equal until sort_by
		//returns (on the assumption that this (1) won't trigger any endless loops, and (2) will
		//enable sort_by to return as early as possible), and then return the error.
		let mut error: Option<GError> = None;
		vec.sort_by(|a, b| {
			if error.is_some() {
				return Ordering::Equal
			}

			match f(a, b) {
				Ok(ordering) => ordering,
				Err(err) => {
					error = Some(err);
					Ordering::Equal
				}
			}
		});

		//copy the sorted values back into our storage
		match error {
			Some(error) => Err(error),
			None => {
				for (i, ch) in vec.drain(..).enumerate() {
					self.set(i, ch).unwrap();
				}

				Ok(())
			}
		}
	}

	fn push<C: IntoElement<char>>(&self, ch: C) -> GResult<()> {
		let ch = ch.into_item()?;
		let ch = &ch;

		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (ch), {
				vec.push_back(ch);
			});

			Ok(())
		})
	}
	
	fn pop<R: FromElement<char>>(&self) -> GResult<R> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			match vec.pop_back() {
				Some(char_storage) => R::from_item(&char_storage.into_char()),
				None => bail!("attempted to pop a char from an empty str")
			}
		})
	}
	
	fn push_start<C: IntoElement<char>>(&self, ch: C) -> GResult<()> {
		let ch = ch.into_item()?;
		let ch = &ch;

		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (ch), {
				vec.push_front(ch);
			});

			Ok(())
		})
	}
	
	fn pop_start<R: FromElement<char>>(&self) -> GResult<R> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			match vec.pop_front() {
				Some(char_storage) => R::from_item(&char_storage.into_char()),
				None => bail!("attempted to pop from the start of an empty str")
			}
		})
	}
	
	fn grow<C: IntoElement<char>>(&self, start_to_add: usize, 
	                          end_to_add: usize, fill: C) -> GResult<()> {
		let fill = fill.into_item()?;
		let fill = &fill;

		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (fill), {
				generic_grow(vec, start_to_add, end_to_add, fill);
			});

			Ok(())
		})
	}
	
	fn shrink(&self, start_to_remove: usize, end_to_remove: usize) -> GResult<()> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			generic_shrink(vec, start_to_remove, end_to_remove)
		})
	}
	
	fn append(&self, other: &Str) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|self_storage| {
			self_storage.append(&other.borrow());
			Ok(())
		})
	}
	
	fn prepend(&self, other: &Str) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|self_storage| {
			self_storage.prepend(&other.borrow());
			Ok(())
		})
	}

	fn resize<C: IntoElement<char>>(&self, new_len: usize, ch: C) -> GResult<()> {
		let ch = ch.into_item()?;
		let ch = &ch;

		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (ch), {
				vec.resize(new_len, ch);
			});

			Ok(())
		})
	}

	fn rotate_left(&self, mid: usize) -> GResult<()> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.rotate_left(mid);
			Ok(())
		})
	}

	fn rotate_right(&self, k: usize) -> GResult<()> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.rotate_right(k);
			Ok(())
		})
	}

	fn swap<I1, I2>(&self, i_index: I1, j_index: I2) -> GResult<()> 
	where 
		I1: DequeIndex, 
		I2: DequeIndex 
	{
		let i = i_index.as_usize(self)?;
		let j = j_index.as_usize(self)?;
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.swap(i, j);
			Ok(())
		})
	}

	fn capacity(&self) -> usize {
		with_str_storage!(&*self.borrow(), vec, (), {
			vec.capacity()
		})
	}

	fn len(&self) -> usize {
		with_str_storage!(&*self.borrow(), vec, (), {
			vec.len()
		})
	}

	fn reserve_exact(&self, additional: usize) -> GResult<()> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.reserve_exact(additional);
			Ok(())
		})
	}

	fn reserve(&self, additional: usize) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (), {
				vec.reserve(additional);
			});

			Ok(())
		})
	}

	fn shrink_to_fit(&self) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (), {
				vec.shrink_to_fit();
			});

			Ok(())
		})
	}

	fn truncate(&self, len: usize) -> GResult<()> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.truncate(len);
			Ok(())
		})
	}

	fn clear(&self) -> GResult<()> {
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.clear();
			Ok(())
		})
	}

	fn contains<C: IntoElement<char>>(&self, ch: C) -> GResult<bool> {
		let ch = ch.into_item()?;
		let ch = &ch;

		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (ch), {
				Ok(vec.contains(&ch))
			})
		})
	}

	#[doc(hidden)]
	fn lock(&self) -> Lock {
		Lock::Str(self.borrow())
	}

	fn extend<I, C>(&self, source: I) -> GResult<()>
	where 
		I: IntoIterator<Item = C>, 
		C: IntoElement<char>
	{
		//there's no way for us to know in advance how much storage we'll need, so we just
		//have to check every character individually... not ideal
		for ch in source.into_iter() {
			let ch = ch.into_item()?;
			let ch = &ch;

			self.borrow_mut_with_capacity_guard(|storage| {
				with_str_storage_mut!(storage, vec, (ch), {
					vec.push_back(ch);
				});

				Ok(())
			})?;
		}

		Ok(())
    }
}

impl<I: DequeIndex> DequeAccess<I> for Str {
	fn get<R: FromElement<char>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		with_str_storage!(&*self.borrow(), vec, (), {
			R::from_item(&vec[i].into_char())
		})
	}

	fn set<C: IntoElement<char>>(&self, index: I, ch: C) -> GResult<()> {
		let i = index.as_usize(self)?;
		let ch = ch.into_item()?;
		let ch = &ch;
		
		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (ch), {
				vec[i] = ch;
			});

			Ok(())
		})
	}

	fn insert<C: IntoElement<char>>(&self, index: I, ch: C) -> GResult<()> {
		let i = index.as_usize_excluded(self)?;
		let ch = ch.into_item()?;
		let ch = &ch;

		self.borrow_mut_with_capacity_guard(|storage| {
			with_str_storage_mut!(storage, vec, (ch), {
				vec.insert(i, ch);
			});

			Ok(())
		})
	}

	fn del(&self, index: I) -> GResult<()> {
		let i = index.as_usize(self)?;
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.remove(i).unwrap();
		});
		Ok(())
	}
	
	fn remove<R: FromElement<char>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			R::from_item(&vec.remove(i).unwrap().into_char())
		})
	}

	fn swap_remove<R: FromElement<char>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			R::from_item(&vec.swap_remove_back(i).unwrap().into_char())
		})
	}

	fn swap_remove_start<R: FromElement<char>>(&self, index: I) -> GResult<R> {
		let i = index.as_usize(self)?;
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			R::from_item(&vec.swap_remove_front(i).unwrap().into_char())
		})
	}
}

impl<I: DequeIndex, R: DequeRange<I> + Debug> DequeAccessRange<I, R> for Str {
	fn del_slice(&self, range: R) -> GResult<()> {
		let r = range.as_range(self)?;
		with_str_storage_mut!(&mut *self.borrow_mut()?, vec, (), {
			vec.drain(r);
		});

		Ok(())
	}
}


//-------------------------------------------------------------------------------------------------
// Deque
//-------------------------------------------------------------------------------------------------

/**
A type-erased `deque`.

Because this type implements the [`DequeOps`](trait.DequeOps.html) trait, you can manipulate it
directly, without needing to access the underlying [`Root<Arr>`](struct.Arr.html) or
[`Root<Str>`](struct.Str.html).
*/

#[derive(Clone, PartialEq, Debug)]
pub enum Deque {
	Arr(Root<Arr>),
	Str(Root<Str>)
}

impl Deque {
	#[doc(hidden)]
	pub fn new(base: &Deque) -> Deque {
		match base {
			Deque::Arr(_) => Deque::Arr(glsp::arr()),
			Deque::Str(_) => Deque::Str(glsp::str())
		}
	}

	#[doc(hidden)]
	pub fn with_capacity(base: &Deque, cap: usize) -> Deque {
		match base {
			Deque::Arr(_) => Deque::Arr(glsp::arr_with_capacity(cap)),
			Deque::Str(_) => Deque::Str(glsp::str_with_capacity(cap))
		}
	}

	#[doc(hidden)]
	pub fn from_iter<T>(base: &Deque, source: T) -> GResult<Deque>
	where
		T: IntoIterator,
		T::Item: ToVal + IntoElement<char>
	{
		match base {
			Deque::Arr(_) => Ok(Deque::Arr(glsp::arr_from_iter(source)?)),
			Deque::Str(_) => Ok(Deque::Str(glsp::str_from_iter(source)?))
		}
	}

	#[doc(hidden)]
	pub fn fill(&self) -> Val {
		match self {
			Deque::Arr(_) => Val::Nil,
			Deque::Str(_) => Val::Char('\0')
		}
	}

	/**
	Returns `true` if both deques share the same identity.

	Equivalent to [`(same? deq other)`](https://gamelisp.rs/std/same-p).
	*/
	pub fn same(&self, other: &Deque) -> bool {
		match (self.clone(), other.clone()) {
			(Deque::Arr(a0), Deque::Arr(a1)) => Val::Arr(a0).same(&Val::Arr(a1)),
			(Deque::Str(s0), Deque::Str(s1)) => Val::Str(s0).same(&Val::Str(s1)),
			_ => false
		}
	}

	/**
	Creates a shallow copy of the deque.

	Equivalent to [`(clone deq)`](https://gamelisp.rs/std/clone).
	*/
	pub fn shallow_clone(&self) -> Deque {
		match self {
			Deque::Arr(ar) => Deque::Arr(ar.shallow_clone()),
			Deque::Str(st) => Deque::Str(st.shallow_clone())
		}
	}

	/**
	Recursively copies the deque and all of its contents.

	Equivalent to [`(deep-clone deq)`](https://gamelisp.rs/std/deep-clone).
	*/
	pub fn deep_clone(&self) -> GResult<Deque> {
		match self {
			Deque::Arr(ar) => Ok(Deque::Arr(ar.deep_clone()?)),
			Deque::Str(st) => Ok(Deque::Str(st.shallow_clone()))
		}
	}

	/**
	Creates an indexing iterator for this collection.

	Equivalent to [`[deq iter]`](https://gamelisp.rs/std/access).
	*/
	pub fn access_giter(&self, giter: &Root<GIter>) -> Root<GIter> {
		match self {
			Deque::Arr(ar) => Arr::access_giter(ar, giter),
			Deque::Str(st) => Str::access_giter(st, giter)
		}
	}
}

impl DequeOps for Deque {
	type Element = Slot;
	type Item = Val;

	fn freeze(&self) {
		match self {
			Deque::Arr(ar) => ar.freeze(),
			Deque::Str(st) => st.freeze()
		}
	}

	fn deep_freeze(&self) {
		match self {
			Deque::Arr(ar) => ar.deep_freeze(),
			Deque::Str(st) => st.deep_freeze()
		}
	}

	fn is_frozen(&self) -> bool {
		match self {
			Deque::Arr(ar) => ar.is_frozen(),
			Deque::Str(st) => st.is_frozen()
		}
	}

	fn is_deep_frozen(&self) -> bool {
		match self {
			Deque::Arr(ar) => ar.is_deep_frozen(),
			Deque::Str(st) => st.is_deep_frozen()
		}
	}

	fn sort(&self) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.sort(),
			Deque::Str(st) => st.sort()
		}
	}

	fn sort_by<F>(&self, mut f: F) -> GResult<()>
	where
		F: FnMut(&Val, &Val) -> GResult<Ordering>
	{
		match self {
			Deque::Arr(ar) => ar.sort_by(f),
			Deque::Str(st) => st.sort_by(|c0, c1| f(&Val::Char(*c0), &Val::Char(*c1)))
		}
	}

	fn can_mutate(&self) -> bool {
		match self {
			Deque::Arr(ar) => ar.can_mutate(),
			Deque::Str(st) => st.can_mutate()
		}
	}

	fn swap<I1, I2>(&self, i: I1, j: I2) -> GResult<()> where I1: DequeIndex, I2: DequeIndex {
		match self {
			Deque::Arr(ar) => ar.swap(i, j),
			Deque::Str(st) => st.swap(i, j)
		}
	}

	fn len(&self) -> usize {
		match self {
			Deque::Arr(ar) => ar.len(),
			Deque::Str(st) => st.len()
		}
	}

	fn contains<V: IntoElement<Slot>>(&self, v: V) -> GResult<bool> {
		match self {
			Deque::Arr(ar) => ar.contains(v),
			Deque::Str(st) => st.contains(v.into_item()?)
		}
	}

	#[doc(hidden)]
	fn lock(&self) -> Lock {
		match self {
			Deque::Arr(ar) => ar.lock(),
			Deque::Str(st) => st.lock()
		}
	}

	fn push<V: IntoElement<Slot>>(&self, v: V) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.push(v),
			Deque::Str(st) => st.push(v.into_item()?)
		}
	}

	fn pop<R: FromElement<Slot>>(&self) -> GResult<R> {
		match self {
			Deque::Arr(ar) => ar.pop(),
			Deque::Str(st) => R::from_item(&st.pop()?)
		}
	}
	
	fn push_start<V: IntoElement<Slot>>(&self, v: V) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.push_start(v),
			Deque::Str(st) => st.push_start(v.into_item()?)
		}
	}
	
	fn pop_start<R: FromElement<Slot>>(&self) -> GResult<R> {
		match self {
			Deque::Arr(ar) => ar.pop_start(),
			Deque::Str(st) => R::from_item(&st.pop_start()?)
		}
	}

	fn grow<V: IntoElement<Slot>>(&self, start: usize, end: usize, fill: V) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.grow(start, end, fill),
			Deque::Str(st) => st.grow(start, end, fill.into_item()?)
		}
	}

	fn shrink(&self, start: usize, end: usize) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.shrink(start, end),
			Deque::Str(st) => st.shrink(start, end)
		}
	}

	fn append(&self, _other: &Self) -> GResult<()> {
		todo!("this should accept &T: DequeOps, rather than &Self")
	}

	fn prepend(&self, _other: &Self) -> GResult<()> {
		todo!("this should accept &T: DequeOps, rather than &Self")
	}

	fn resize<V: IntoElement<Slot>>(&self, new_len: usize, fill: V) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.resize(new_len, fill),
			Deque::Str(st) => st.resize(new_len, fill.into_item()?)
		}
	}

	fn rotate_left(&self, mid: usize) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.rotate_left(mid),
			Deque::Str(st) => st.rotate_left(mid)
		}
	}
	
	fn rotate_right(&self, k: usize) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.rotate_right(k),
			Deque::Str(st) => st.rotate_right(k)
		}
	}

	fn capacity(&self) -> usize {
		match self {
			Deque::Arr(ar) => ar.capacity(),
			Deque::Str(st) => st.capacity()
		}
	}

	fn reserve_exact(&self, additional: usize) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.reserve_exact(additional),
			Deque::Str(st) => st.reserve_exact(additional)
		}
	}

	fn reserve(&self, additional: usize) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.reserve(additional),
			Deque::Str(st) => st.reserve(additional)
		}
	}

	fn shrink_to_fit(&self) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.shrink_to_fit(),
			Deque::Str(st) => st.shrink_to_fit()
		}
	}

	fn truncate(&self, len: usize) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.truncate(len),
			Deque::Str(st) => st.truncate(len)
		}
	}

	fn clear(&self) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.clear(),
			Deque::Str(st) => st.clear()
		}
	}

	fn extend<I, C>(&self, source: I) -> GResult<()>
	where 
		I: IntoIterator<Item = C>, 
		C: IntoElement<Slot>
	{
		match self {
			Deque::Arr(ar) => ar.extend(source),
			Deque::Str(st) => st.extend(source.into_iter().map(|c| c.into_item().unwrap()))
		}
	}
}

impl<I: DequeIndex> DequeAccess<I> for Deque {
	fn get<R: FromElement<Slot>>(&self, i: I) -> GResult<R> {
		match self {
			Deque::Arr(ar) => ar.get(i),
			Deque::Str(st) => R::from_item(&st.get(i)?)
		}
	}

	fn set<V: IntoElement<Slot>>(&self, i: I, v: V) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.set(i, v),
			Deque::Str(st) => st.set(i, v.into_item()?)
		}
	}

	fn insert<V: IntoElement<Slot>>(&self, i: I, v: V) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.insert(i, v),
			Deque::Str(st) => st.insert(i, v.into_item()?)
		}
	}

	fn del(&self, i: I) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.del(i),
			Deque::Str(st) => st.del(i)
		}
	}

	fn remove<R: FromElement<Slot>>(&self, i: I) -> GResult<R> {
		match self {
			Deque::Arr(ar) => ar.remove(i),
			Deque::Str(st) => R::from_item(&st.remove(i)?)
		}
	}

	fn swap_remove<R: FromElement<Slot>>(&self, i: I) -> GResult<R> {
		match self {
			Deque::Arr(ar) => ar.swap_remove(i),
			Deque::Str(st) => R::from_item(&st.swap_remove(i)?)
		}
	}

	fn swap_remove_start<R: FromElement<Slot>>(&self, i: I) -> GResult<R> {
		match self {
			Deque::Arr(ar) => ar.swap_remove_start(i),
			Deque::Str(st) => R::from_item(&st.swap_remove_start(i)?)
		}
	}
}

impl<I, R> DequeAccessRange<I, R> for Deque
where
	I: DequeIndex, 
	R: DequeRange<I> + Debug
{
	fn del_slice(&self, r: R) -> GResult<()> {
		match self {
			Deque::Arr(ar) => ar.del_slice(r),
			Deque::Str(st) => st.del_slice(r)
		}
	}
}


//-------------------------------------------------------------------------------------------------
// IterDeque, IterDequeTo
//-------------------------------------------------------------------------------------------------

#[doc(hidden)]
pub enum Lock<'a> {
	Arr(Ref<'a, VecDeque<Slot>>),
	Str(Ref<'a, StrStorage>)
}

impl<'a> Clone for Lock<'a> {
	fn clone(&self) -> Lock<'a> {
		match self {
			Lock::Arr(rf) => Lock::Arr(Ref::clone(rf)),
			Lock::Str(rf) => Lock::Str(Ref::clone(rf))
		}
	}
}

/**
An infallible iterator over a deque's contents.

Created by [`DequeOps::iter`](trait.DequeOps.html#method.iter).
*/

pub struct IterDeque<'a, T: DequeAccess<usize>> {
	deq: &'a T,
	_lock: Lock<'a>,
	start_index: usize,
	past_the_end_index: usize
}

impl<'a, T: DequeAccess<usize>,> IterDeque<'a, T> {
	fn from(deq: &'a T) -> IterDeque<'a, T> {
		let lock = deq.lock();

		IterDeque {
			deq,
			_lock: lock,
			start_index: 0,
			past_the_end_index: deq.len()
		}
	}
}

impl<'a, T: DequeAccess<usize>> Iterator for IterDeque<'a, T> {
	type Item = T::Item;

	fn next(&mut self) -> Option<T::Item> {
		if self.start_index < self.past_the_end_index {
			let i = self.start_index;
			self.start_index += 1;
			Some(self.deq.get(i).unwrap())
		} else {
			None
		}
	}

	fn nth(&mut self, n: usize) -> Option<T::Item> {
		self.start_index += n;
		self.next()
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		let size = self.past_the_end_index.saturating_sub(self.start_index);
		(size, Some(size))
	}
}

impl<'a, T: DequeAccess<usize>> DoubleEndedIterator for IterDeque<'a, T> {
	fn next_back(&mut self) -> Option<T::Item> {
		if self.past_the_end_index > self.start_index {
			let i = self.past_the_end_index - 1;
			self.past_the_end_index -= 1;
			Some(self.deq.get(i).unwrap())
		} else {
			None
		}
	}
}

impl<'a, T: DequeAccess<usize>> ExactSizeIterator for IterDeque<'a, T> { }

impl<'a> IntoIterator for &'a Arr {
	type Item = Val;
	type IntoIter = IterDeque<'a, Arr>;

	fn into_iter(self) -> IterDeque<'a, Arr> {
		IterDeque::from(self)
	}
}

impl<'a> IntoIterator for &'a Root<Arr> {
	type Item = Val;
	type IntoIter = IterDeque<'a, Arr>;

	fn into_iter(self) -> IterDeque<'a, Arr> {
		IterDeque::from(&**self)
	}
}

impl<'a> IntoIterator for &'a mut Root<Arr> {
	type Item = Val;
	type IntoIter = IterDeque<'a, Arr>;

	fn into_iter(self) -> IterDeque<'a, Arr> {
		IterDeque::from(&**self)
	}
}

impl<'a> IntoIterator for &'a Str {
	type Item = char;
	type IntoIter = IterDeque<'a, Str>;

	fn into_iter(self) -> IterDeque<'a, Str> {
		IterDeque::from(self)
	}
}

impl<'a> IntoIterator for &'a Root<Str> {
	type Item = char;
	type IntoIter = IterDeque<'a, Str>;

	fn into_iter(self) -> IterDeque<'a, Str> {
		IterDeque::from(&**self)
	}
}

impl<'a> IntoIterator for &'a mut Root<Str> {
	type Item = char;
	type IntoIter = IterDeque<'a, Str>;

	fn into_iter(self) -> IterDeque<'a, Str> {
		IterDeque::from(&**self)
	}
}

/**
A converting iterator over a deque's contents.

Created by [`DequeOps::iter_to`](trait.DequeOps.html#method.iter_to).
*/

pub struct IterDequeTo<'a, T: DequeAccess<usize>, R: FromElement<T::Element>> {
	deq: &'a T,
	_lock: Lock<'a>,
	start_index: usize,
	past_the_end_index: usize,
	phantom: PhantomData<R>
}

impl<'a, T: DequeAccess<usize>, R: FromElement<T::Element>> IterDequeTo<'a, T, R> {
	fn from(deq: &'a T) -> IterDequeTo<'a, T, R> {
		let lock = deq.lock();

		IterDequeTo {
			deq,
			_lock: lock,
			start_index: 0,
			past_the_end_index: deq.len(),
			phantom: PhantomData
		}
	}
}

impl<'a, T: DequeAccess<usize>, R: FromElement<T::Element>> Iterator for IterDequeTo<'a, T, R> {
	type Item = GResult<R>;
	fn next(&mut self) -> Option<GResult<R>> {
		if self.start_index < self.past_the_end_index {
			let i = self.start_index;
			self.start_index += 1;
			Some(self.deq.get(i))
		} else {
			None
		}
	}

	fn nth(&mut self, n: usize) -> Option<GResult<R>> {
		self.start_index += n;
		self.next()
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		let size = self.past_the_end_index.saturating_sub(self.start_index);
		(size, Some(size))
	}
}

impl<'a, T: DequeAccess<usize>, R: FromElement<T::Element>> DoubleEndedIterator for IterDequeTo<'a, T, R> {
	fn next_back(&mut self) -> Option<GResult<R>> {
		if self.past_the_end_index > self.start_index {
			let i = self.past_the_end_index - 1;
			self.past_the_end_index -= 1;
			Some(self.deq.get(i))
		} else {
			None
		}
	}
}

impl<'a, T: DequeAccess<usize>, R: FromElement<T::Element>> ExactSizeIterator for IterDequeTo<'a, T, R> { }


//-------------------------------------------------------------------------------------------------
// Tab
//-------------------------------------------------------------------------------------------------

/**
The `tab` primitive type.

New tables can be constructed using the [`tab!` macro](macro.tab.html) or various toplevel 
functions, such as [`glsp::tab`](fn.tab.html) and [`glsp::tab_from_iter`](fn.tab_from_iter.html).
Tables are always stored on the garbage-collected heap, so they're normally represented by
the type [`Root<Tab>`](struct.Root.html).
*/

pub struct Tab {
	header: GcHeader,
	map: RefCell<FnvHashMap<Slot, Slot>>
}

impl Allocate for Tab {
	fn header(&self) -> &GcHeader {
		&self.header
	}

	fn visit_gcs<V: Visitor>(&self, visitor: &mut V) {
		for (internal_key, internal_value) in self.map.borrow().iter() {
			visitor.visit_slot(internal_key);
			visitor.visit_slot(internal_value);
		}
	}

	fn clear_gcs(&self) {
		self.map.borrow_mut().clear()
	}

	fn owned_memory_usage(&self) -> usize {
		//https://github.com/rust-lang/hashbrown describes the current HashMap implementation
		//as having "1 byte of overhead per bucket", and the source code currently requires
		//one in eight buckets to be free. see hashbrown::raw, mod.rs, line 179.
		let cap = self.map.borrow().capacity();
		let buckets = if cap < 8 {
			cap + 1
		} else {
			(cap * 8) / 7
		};

		buckets.next_power_of_two() * (1 + size_of::<(Slot, Slot)>())
	}
}

/**
A non-panicking version of [`tab!`](macro.tab.html).

Returns a [`GResult<Root<Tab>>`](struct.Tab.html).
*/

//we're stuck with `key => val` syntax, rather than `key: val`, because exprs can only be
//followed by , ; or =>

#[macro_export]
macro_rules! try_tab {
	() => ($crate::GResult::Ok($crate::tab()));
	($(($key:expr, $value:expr),)* ..$base:expr) => (
		{
			//we only use this loop so that we can break with a value (`try` blocks are nyi)
        	loop {
				let tab = $base.shallow_clone();
				$(
					if let Err(err) = tab.set($key, $value) {
						break $crate::GResult::Err(err)
					}
				)*
				break $crate::GResult::Ok(tab)
			}
		}
	);
	($(($key:expr, $value:expr),)*) => (
        {
        	//we only use this loop so that we can break with a value (`try` blocks are nyi)
        	loop {
        		let tab = $crate::tab();
        		$(
					if let Err(err) = tab.set($key, $value) {
						break $crate::GResult::Err(err)
					}
				)*
				break $crate::GResult::Ok(tab)
	        }
        }
    );
    ($(($key:expr, $value:expr)),*) => (
		$crate::try_tab! {
			$($key => $value,)*
		}
	);
}

/**
Constructs a table.

The input syntax is any number of `(key, value)` pairs, each surrounded by parentheses. They
can optionally be followed with a single "base" argument, prefixed with `..`, which must evaluate 
to a `&Tab` or a `Root<Tab>`.

The return type is [`Root<Tab>`](struct.Tab.html).
	
	let original = tab! {
		(glsp::sym("a")?, 0),
		(glsp::sym("b")?, 1)
	};

	let table = tab! {
		(glsp::sym("b")?, 1),
		(glsp::sym("c")?, 2),
		..original
	};

	println!("{}", table.len()); //prints 3

In the unlikely event that a key or value fails to be converted to a [`Val`](enum.Val.html), the 
macro will panic. [`try_tab!`](macro.try_tab.html) is the non-panicking alternative.
*/

#[macro_export]
macro_rules! tab {
	() => ($crate::try_tab!{}.unwrap());
	($(($key:expr, $value:expr),)* ..$base:expr) => (
		$crate::try_tab! {
			$(($key, $value),)*
			..$base
		}.unwrap()
	);
	($(($key:expr, $value:expr),)*) => (
		$crate::try_tab! {
			$(($key, $value),)*
		}.unwrap()
	);
	($(($key:expr, $value:expr)),*) => (
		$crate::try_tab! {
			$(($key, $value),)*
		}.unwrap()
	);
}

impl Tab {
	pub(crate) fn new() -> Tab {
		Tab {
			header: GcHeader::new(),
			map: RefCell::new(FnvHashMap::default())
		}
	}

	pub(crate) fn from_iter<T, K, V>(t: T) -> GResult<Tab> 
	where
		T: IntoIterator<Item = (K, V)>,
		K: ToVal,
		V: ToVal
	{
		let iter = t.into_iter();
		let mut map = FnvHashMap::with_capacity_and_hasher(iter.size_hint().0, Default::default());

		for (key, value) in iter {
			map.insert(key.to_slot()?, value.to_slot()?);
		}

		Ok(Tab {
			header: GcHeader::new(),
			map: RefCell::new(map)
		})
	}

	pub(crate) fn with_capacity(capacity: usize) -> Tab {
		Tab {
			header: GcHeader::new(),
			map: RefCell::new(FnvHashMap::with_capacity_and_hasher(capacity, Default::default()))
		}
	}

	pub(crate) fn borrow(&self) -> Ref<FnvHashMap<Slot, Slot>> {
		self.map.borrow()
	}

	pub(crate) fn borrow_mut(&self) -> GResult<RefMut<FnvHashMap<Slot, Slot>>> {
		ensure!(!self.header.frozen(), "attempted to mutate a frozen tab");
		match self.map.try_borrow_mut() {
			Ok(ref_mut) => Ok(ref_mut),
			Err(_) => bail!("attempted to mutate a borrowed tab")
		}
	}

	/**
	Creates a shallow copy of the table.

	Equivalent to [`(clone t)`](https://gamelisp.rs/std/clone).
	*/
	pub fn shallow_clone(&self) -> Root<Tab> {
		glsp::tab_from_iter(self.entries().iter()).unwrap()
	}

	/**
	Recursively copies the table and all of its contents.

	Equivalent to [`(deep-clone t)`](https://gamelisp.rs/std/deep-clone).
	*/
	//todo: check for reference cycles
	pub fn deep_clone(&self) -> GResult<Root<Tab>> {
		let tab = glsp::tab_with_capacity(self.len());
		for (k, v) in self.entries().iter() {
			tab.set(k.deep_clone()?, v.deep_clone()?)?;
		}
		Ok(tab)
	}

	/**
	Equivalent to [`(eq? self other)`](https://gamelisp.rs/std/eq-p).

	Note that, because this method may need to invoke an `op-eq?` method when one of the
	table's values is an object or an `RData`, it can potentially fail.

	The same is true for `PartialEq` comparisons between tables using Rust's `==` operator.
	In that case, if an error occurs, the operator will panic.
	*/
	pub fn try_eq(&self, other: &Tab) -> GResult<bool> {
		if self.len() != other.len() {
			return Ok(false)
		}

		for (k0, v0) in self.entries().iter() {
			let v1: Option<Val> = other.get_if_present(&k0).unwrap();
			if v1.is_none() || !v1.unwrap().try_eq(&v0)? {
				return Ok(false)
			}
		}

		Ok(true)
	}

	#[allow(dead_code)]
	fn write_barrier_val(&self, val: &Val) {
		with_heap(|heap| heap.write_barrier_val(self, val));
	}

	fn write_barrier_slot(&self, slot: &Slot) {
		with_heap(|heap| heap.write_barrier_slot(self, slot));
	}

	fn memory_usage_barrier(&self, prev_usage: usize, cur_usage: usize) {
		with_heap(|heap| heap.memory_usage_barrier(self, prev_usage, cur_usage));
	}

	fn borrow_mut_with_capacity_guard<R, F>(&self, f: F) -> GResult<R> 
	where
		F: FnOnce(&mut RefMut<FnvHashMap<Slot, Slot>>) -> GResult<R>
	{
		let prev_usage = self.owned_memory_usage();
		let _guard = Guard::new(|| {
			let cur_usage = self.owned_memory_usage();
			if prev_usage != cur_usage {
				self.memory_usage_barrier(prev_usage, cur_usage);
			}
		});

		let mut ref_mut = self.borrow_mut()?;

		f(&mut ref_mut)
	}

	/**
	Indexes the table.

	Equivalent to [`[t key]`](https://gamelisp.rs/std/access).
	*/
	pub fn get<K: ToVal, V: FromVal>(&self, key: K) -> GResult<V> {
		let key = key.to_slot()?;
		match self.borrow().get(&key) {
			Some(value) => V::from_slot(value),
			None => bail!("missing tab field {:?}", key)
		}
	}

	/**
	Indexes the table, if the given key is present.

	Equivalent to [`[t (? key)]`](https://gamelisp.rs/std/access).
	*/
	pub fn get_if_present<K: ToVal, V: FromVal>(&self, key: K) -> GResult<Option<V>> {
		match self.borrow().get(&key.to_slot()?) {
			Some(value) => Ok(Some(V::from_slot(value)?)),
			None => Ok(None)
		}
	}

	/**
	Mutates the value stored at the given key, or inserts a new key/value pair.

	Equivalent to [`(= [t key] value)`](https://gamelisp.rs/std/set-access).
	*/
	pub fn set<K, V>(&self, key: K, value: V) -> GResult<()> 
	where
		K: ToVal,
		V: ToVal
	{
		let key = key.to_slot()?;
		let value = value.to_slot()?;

		self.borrow_mut_with_capacity_guard(|map| {
			self.write_barrier_slot(&key);
			self.write_barrier_slot(&value);
			map.insert(key, value);
			Ok(())
		})
	}

	/**
	Mutates the value stored at the given key, if the key is already present in the table.

	Returns `true` if the key was present.

	Equivalent to [`(= [t (? key)] value)`](https://gamelisp.rs/std/set-access).
	*/
	pub fn set_if_present<K, V>(&self, key: K, value: V) -> GResult<bool> 
	where
		K: ToVal,
		V: ToVal
	{
		let key = key.to_slot()?;
		let value = value.to_slot()?;

		self.borrow_mut_with_capacity_guard(|map| {
			match map.get_mut(&key) {
				Some(dst_value) => {
					self.write_barrier_slot(&key);
					self.write_barrier_slot(&value);
					*dst_value = value;
					Ok(true)
				}
				None => Ok(false)
			}
		})
	}

	/**
	Returns `true` if the given key is present in the table.

	Equivalent to [`(has? t key)`](https://gamelisp.rs/std/has-p).
	*/
	pub fn has<K: ToVal>(&self, key: K) -> GResult<bool> {
		let key = key.to_slot()?;
		Ok(self.borrow().contains_key(&key))
	}

	/**
	Removes a key/value pair from the table, without returning it.

	Equivalent to [`(del! t key)`](https://gamelisp.rs/std/del-mut).
	*/
	pub fn del<K: ToVal>(&self, key: K) -> GResult<()> {
		let key = key.to_slot()?;
		match self.borrow_mut()?.remove(&key) {
			Some(_) => Ok(()),
			None => bail!("attempted to delete nonexistent tab field {}", key)
		}
	}

	/**
	Removes a key/value pair from the table if it's present, without returning it.

	Returns `true` if the key was present.

	Equivalent to [`(del! t (? key))`](https://gamelisp.rs/std/del-mut).
	*/
	pub fn del_if_present<K: ToVal>(&self, key: K) -> GResult<bool> {
		let key = key.to_slot()?;
		Ok(self.borrow_mut()?.remove(&key).is_some())
	}

	/**
	Removes a key/value pair from the table and returns it.

	Equivalent to [`(remove! t key)`](https://gamelisp.rs/std/remove-mut).
	*/
	pub fn remove<K: ToVal, V: FromVal>(&self, key: K) -> GResult<V> {
		let key = key.to_slot()?;
		match self.borrow_mut()?.remove(&key) {
			Some(value) => V::from_slot(&value),
			None => bail!("attempted to remove nonexistent tab field {}", key)
		}
	}

	/**
	Removes a key/value pair from the table and returns it, if it's present.

	Equivalent to [`(remove! t (? key))`](https://gamelisp.rs/std/remove-mut).
	*/
	pub fn remove_if_present<K: ToVal, V: FromVal>(&self, key: K) -> GResult<Option<V>> {
		let key = key.to_slot()?;
		match self.borrow_mut()?.remove(&key) {
			Some(value) => Ok(Some(V::from_slot(&value)?)),
			None => Ok(None)
		}
	}

	/**
	Returns the table's length.

	Equivalent to [`(len t)`](https://gamelisp.rs/std/len).
	*/
	pub fn len(&self) -> usize {
		self.borrow().len()
	}

	/**
	Returns the table's storage capacity.

	Equivalent to [`HashMap::capacity`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.HashMap.html#method.capacity
	*/
	pub fn capacity(&self) -> usize {
		self.borrow().capacity()
	}

	/**
	Reserves enough space for at least `additional` elements to be added to the table.

	Equivalent to [`HashMap::reserve`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.HashMap.html#method.reserve
	*/
	pub fn reserve(&self, additional: usize) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|map| {
			map.reserve(additional);
			Ok(())
		})
	}

	/**
	Shrinks the capacity of the table as much as possible.

	Equivalent to [`HashMap::shrink_to_fit`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.HashMap.html#method.shrink_to_fit
	*/
	pub fn shrink_to_fit(&self) -> GResult<()> {
		self.borrow_mut_with_capacity_guard(|map| {
			map.shrink_to_fit();
			Ok(())
		})
	}

	/**
	Removes all of the table's elements.

	Equivalent to [`HashMap::clear`][0].

	[0]: https://doc.rust-lang.org/std/collections/struct.HashMap.html#method.clear
	*/
	pub fn clear(&self) -> GResult<()> {
		self.borrow_mut()?.clear();
		Ok(())
	}

	/**
	Returns an adapter which can be used to construct iterators over the table's contents.
	*/

	//without self-referential structs or `unsafe` code, it's impossible for us to convert a
	//&Tab into an Iterator. therefore, we can't implement IntoIterator for &Tab, nor can we
	//provide an iter() method. instead, we require the user to call entries(), which has methods
	//iter(), keys() and values(). 
	pub fn entries<'a>(&'a self) -> TabEntries<'a> {
		TabEntries(self.borrow())
	}

	/**
	Returns a [`Root<GIter>`](struct.GIter.html) which iterates over the table's keys.

	Equivalent to [`(keys t)`](https://gamelisp.rs/std/keys).
	*/
	pub fn gkeys(&self) -> Root<GIter> {
		let arr = glsp::arr_with_capacity(self.len());
		for key in self.entries().keys_to::<Slot>() {
			arr.push(key.unwrap()).unwrap();
		}

		glsp::giter(GIterState::TabKeys(arr.to_gc()))
	}

	/**
	Returns a [`Root<GIter>`](struct.GIter.html) which iterates over the table's values.

	Equivalent to [`(values t)`](https://gamelisp.rs/std/values).
	*/
	pub fn gvalues(&self) -> Root<GIter> {
		let arr = glsp::arr_with_capacity(self.len());
		for value in self.entries().values_to::<Slot>() {
			arr.push(value.unwrap()).unwrap();
		}
		
		glsp::giter(GIterState::TabValues(arr.to_gc()))
	}

	/**
	Inserts the contents of an iterator as table entries.

	Existing entries will be silently overwritten.
	*/
	pub fn extend<T, K, V>(&mut self, iter_source: T) -> GResult<()>
	where 
		T: IntoIterator<Item = (K, V)>,
		K: ToVal,
		V: ToVal
	{
		let iter = iter_source.into_iter();

		self.borrow_mut_with_capacity_guard(|map| {
			map.reserve(iter.size_hint().0);
			Ok(())
		})?;

		for (key, value) in iter {
			let key = key.to_slot()?;
			let value = value.to_slot()?;

			self.write_barrier_slot(&key);
			self.write_barrier_slot(&value);

			self.borrow_mut()?.insert(key, value);
		}

		Ok(())
    }

    /**
	Makes the table immutable.

	Equivalent to [`(freeze! t)`](https://gamelisp.rs/std/freeze-mut).
	*/
    pub fn freeze(&self) {
		self.header.freeze()
	}

	/**
	Makes the table and all of its contents immutable.

	Equivalent to [`(deep-freeze! t)`](https://gamelisp.rs/std/deep-freeze-mut).
	*/

	//todo: handle reference cycles
	pub fn deep_freeze(&self) {
		self.freeze();
		for (key, value) in self.entries().iter() {
			key.deep_freeze();
			value.deep_freeze();
		}
	}

	///Returns `true` if the table has been frozen.
	pub fn is_frozen(&self) -> bool {
		self.header.frozen()
	}

	//todo: handle reference cycles
	///Returns `true` if the table and all of its contents have been frozen.
	pub fn is_deep_frozen(&self) -> bool {
		self.is_frozen() && self.entries().iter().all(|(key, value)| {
			key.is_deep_frozen() && value.is_deep_frozen()
		})
	}

	/**
	Returns `true` if it's possible to mutate the table without triggering an error.
	
	This method will currently return `false` if the table has been frozen, or if it's
	currently being iterated from Rust.
	*/
	pub fn can_mutate(&self) -> bool {
		(!self.header.frozen()) && self.map.try_borrow_mut().is_ok()
	}
}

impl PartialEq<Tab> for Tab {
	fn eq(&self, other: &Tab) -> bool {
		self.try_eq(other).unwrap()
	}
}

impl Eq for Tab { }


//-------------------------------------------------------------------------------------------------
// TabEntries, IterTab, IterKeys, IterValues
//-------------------------------------------------------------------------------------------------

/**
An adapter struct which can be used to iterate over the contents of a table.

Created by [`Tab::entries`](struct.Tab.html#method.entries).
*/
pub struct TabEntries<'a>(Ref<'a, FnvHashMap<Slot, Slot>>);

impl<'a> TabEntries<'a> {
	/**
	Creates an infallible iterator over this table's `(key, value)` pairs.
	
	The iterator's item type will be [`(Val, Val)`](enum.Val.html).
	*/
	pub fn iter<'b>(&'b self) -> IterTab<'a, 'b> {
		IterTab::from(self)
	}

	/**
	Creates a converting iterator over this table's `(key, value)` pairs.
	
	The iterator can produce any tuple `(K, V)`, where both `K` and `V` implement 
	[`FromVal`](trait.FromVal.html). Each key/value pair will be wrapped as
	[`GResult<(K, V)>`](type.GResult.html), so it will need to be unwrapped using `?` 
	before it can be used.
	*/
	pub fn iter_to<'b, K: FromVal, V: FromVal>(&'b self) -> IterTabTo<'a, 'b, K, V> {
		IterTabTo::from(self)
	}

	/**
	Creates an infallible iterator over this table's keys.
	
	The iterator's item type will be [`Val`](enum.Val.html).
	*/
	pub fn keys<'b>(&'b self) -> IterTabKeys<'a, 'b> {
		IterTabKeys::from(self)
	}

	/**
	Creates a converting iterator over this table's keys.
	
	The iterator can produce any type which implements [`FromVal`](trait.FromVal.html). 
	Each key will be wrapped as a [`GResult<K>`](type.GResult.html), so it will need to 
	be unwrapped using `?` before it can be used.
	*/
	pub fn keys_to<'b, K: FromVal>(&'b self) -> IterTabKeysTo<'a, 'b, K> {
		IterTabKeysTo::from(self)
	}

	/**
	Creates an infallible iterator over this table's values.
	
	The iterator's item type will be [`Val`](enum.Val.html).
	*/
	pub fn values<'b>(&'b self) -> IterTabValues<'a, 'b> {
		IterTabValues::from(self)
	}

	/**
	Creates a converting iterator over this table's values.
	
	The iterator can produce any type which implements [`FromVal`](trait.FromVal.html). 
	Each key will be wrapped as a [`GResult<V>`](type.GResult.html), so it will need to 
	be unwrapped using `?` before it can be used.
	*/
	pub fn values_to<'b, V: FromVal>(&'b self) -> IterTabValuesTo<'a, 'b, V> {
		IterTabValuesTo::from(self)
	}
}

impl<'a, 'b> IntoIterator for &'b TabEntries<'a> {
	type Item = (Val, Val);
	type IntoIter = IterTab<'a, 'b>;

	fn into_iter(self) -> IterTab<'a, 'b> {
		IterTab::from(self)
	}
}

/**
An infallible iterator over a table's `(key, value)` pairs.

Created by [`TabEntries::iter`](struct.TabEntries.html#method.iter).
*/

pub struct IterTab<'a, 'b> {
	entries: PhantomData<&'b TabEntries<'a>>,
	iter: hash_map::Iter<'b, Slot, Slot>
}

impl<'a, 'b> IterTab<'a, 'b> {
	fn from(entries: &'b TabEntries<'a>) -> Self {
		IterTab {
			entries: PhantomData,
			iter: (entries.0).iter()
		}
	}
}

impl<'a, 'b> Iterator for IterTab<'a, 'b> {
	type Item = (Val, Val);

	fn next(&mut self) -> Option<(Val, Val)> {
		self.iter.next().map(|(key, value)| {
			(key.root(), value.root())
		})
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.iter.size_hint()
	}
}

impl<'a, 'b> ExactSizeIterator for IterTab<'a, 'b> { }

impl<'a, 'b> FusedIterator for IterTab<'a, 'b> { }

/**
A converting iterator over a table's `(key, value)` pairs.

Created by [`TabEntries::iter_to`](struct.TabEntries.html#method.iter_to).
*/

pub struct IterTabTo<'a, 'b, K: FromVal, V: FromVal> {
	entries: PhantomData<&'b TabEntries<'a>>,
	iter: hash_map::Iter<'b, Slot, Slot>,
	phantom: PhantomData<(K, V)>
}

impl<'a, 'b, K: FromVal, V: FromVal> IterTabTo<'a, 'b, K, V> {
	fn from(entries: &'b TabEntries<'a>) -> Self {
		IterTabTo {
			entries: PhantomData,
			iter: (entries.0).iter(),
			phantom: PhantomData
		}
	}
}

impl<'a, 'b, K: FromVal, V: FromVal> Iterator for IterTabTo<'a, 'b, K, V> {
	type Item = GResult<(K, V)>;

	fn next(&mut self) -> Option<GResult<(K, V)>> {
		self.iter.next().map(|(internal_key, internal_value)| {
			let key = K::from_slot(internal_key)?;
			let value = V::from_slot(internal_value)?;
			Ok((key, value))
		})
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.iter.size_hint()
	}
}

impl<'a, 'b, K: FromVal, V: FromVal> ExactSizeIterator for IterTabTo<'a, 'b, K, V> { }

impl<'a, 'b, K: FromVal, V: FromVal> FusedIterator for IterTabTo<'a, 'b, K, V> { }

/**
An infallible iterator over a table's keys.

Created by [`TabEntries::keys`](struct.TabEntries.html#method.keys).
*/

pub struct IterTabKeys<'a, 'b> {
	entries: PhantomData<&'b TabEntries<'a>>,
	iter: hash_map::Keys<'b, Slot, Slot>
}

impl<'a, 'b> IterTabKeys<'a, 'b> {
	fn from(entries: &'b TabEntries<'a>) -> Self {
		IterTabKeys {
			entries: PhantomData,
			iter: (entries.0).keys()
		}
	}
}

impl<'a, 'b> Iterator for IterTabKeys<'a, 'b> {
	type Item = Val;

	fn next(&mut self) -> Option<Val> {
		self.iter.next().map(|internal_key| internal_key.root())
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.iter.size_hint()
	}
}

impl<'a, 'b> ExactSizeIterator for IterTabKeys<'a, 'b> { }

impl<'a, 'b> FusedIterator for IterTabKeys<'a, 'b> { }

/**
A converting iterator over a table's keys.

Created by [`TabEntries::keys_to`](struct.TabEntries.html#method.keys_to).
*/

pub struct IterTabKeysTo<'a, 'b, K: FromVal> {
	entries: PhantomData<&'b TabEntries<'a>>,
	iter: hash_map::Keys<'b, Slot, Slot>,
	phantom: PhantomData<K>
}

impl<'a, 'b, K: FromVal> IterTabKeysTo<'a, 'b, K> {
	fn from(entries: &'b TabEntries<'a>) -> Self {
		IterTabKeysTo {
			entries: PhantomData,
			iter: (entries.0).keys(),
			phantom: PhantomData
		}
	}
}

impl<'a, 'b, K: FromVal> Iterator for IterTabKeysTo<'a, 'b, K> {
	type Item = GResult<K>;

	fn next(&mut self) -> Option<GResult<K>> {
		self.iter.next().map(|internal_key| {
			K::from_slot(internal_key)
		})
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.iter.size_hint()
	}
}

impl<'a, 'b, K: FromVal> ExactSizeIterator for IterTabKeysTo<'a, 'b, K> { }

impl<'a, 'b, K: FromVal> FusedIterator for IterTabKeysTo<'a, 'b, K> { }

/**
An infallible iterator over a table's values.

Created by [`TabEntries::values`](struct.TabEntries.html#method.values).
*/

pub struct IterTabValues<'a, 'b> {
	entries: PhantomData<&'b TabEntries<'a>>,
	iter: hash_map::Values<'b, Slot, Slot>
}

impl<'a, 'b> IterTabValues<'a, 'b> {
	fn from(entries: &'b TabEntries<'a>) -> Self {
		IterTabValues {
			entries: PhantomData,
			iter: (entries.0).values()
		}
	}
}

impl<'a, 'b> Iterator for IterTabValues<'a, 'b> {
	type Item = Val;

	fn next(&mut self) -> Option<Val> {
		self.iter.next().map(|internal_value| internal_value.root())
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.iter.size_hint()
	}
}

impl<'a, 'b> ExactSizeIterator for IterTabValues<'a, 'b> { }

impl<'a, 'b> FusedIterator for IterTabValues<'a, 'b> { }

/**
A converting iterator over a table's values.

Created by [`TabEntries::values_to`](struct.TabEntries.html#method.values_to).
*/

pub struct IterTabValuesTo<'a, 'b, V: FromVal> {
	entries: PhantomData<&'b TabEntries<'a>>,
	iter: hash_map::Values<'b, Slot, Slot>,
	phantom: PhantomData<V>
}

impl<'a, 'b, V: FromVal> IterTabValuesTo<'a, 'b, V> {
	fn from(entries: &'b TabEntries<'a>) -> Self {
		IterTabValuesTo {
			entries: PhantomData,
			iter: (entries.0).values(),
			phantom: PhantomData
		}
	}
}

impl<'a, 'b, V: FromVal> Iterator for IterTabValuesTo<'a, 'b, V> {
	type Item = GResult<V>;

	fn next(&mut self) -> Option<GResult<V>> {
		self.iter.next().map(|internal_value| {
			V::from_slot(internal_value)
		})
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.iter.size_hint()
	}
}

impl<'a, 'b, V: FromVal> ExactSizeIterator for IterTabValuesTo<'a, 'b, V> { }

impl<'a, 'b, V: FromVal> FusedIterator for IterTabValuesTo<'a, 'b, V> { }

