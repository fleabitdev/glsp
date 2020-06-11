use std::char;
use std::cmp::{Ordering, PartialOrd};
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::num::{FpCategory};
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};
use super::class::{Class, Obj};
use super::code::{Coro, GFn};
use super::collections::{Arr, DequeOps, Str, Tab};
use super::engine::{RFn, RData, stock_syms::*, Sym};
use super::error::{GResult};
use super::gc::Root;
use super::iter::{GIter};


//-------------------------------------------------------------------------------------------------
// Val
//-------------------------------------------------------------------------------------------------

/**
Any GameLisp value.

Many functions in this crate provide automatic conversions to and from `Val`, using the 
[`FromVal`](trait.FromVal.html) and [`ToVal`](trait.ToVal.html) traits.
*/

#[derive(Clone)]
pub enum Val {
	Nil,
	Int(i32),
	Flo(f32),
	Char(char),
	Bool(bool),
	Sym(Sym),
	RFn(RFn),
	Arr(Root<Arr>),
	Str(Root<Str>),
	Tab(Root<Tab>),
	GIter(Root<GIter>),
	Obj(Root<Obj>),
	Class(Root<Class>),
	GFn(Root<GFn>),
	Coro(Root<Coro>),
	RData(Root<RData>),
}

impl Default for Val {
	fn default() -> Val {
		Val::Nil
	}
}

macro_rules! impl_val {
	($(($variant:ident, $type:ty, $type_name:literal, $a_type_name:literal, $is_type:ident, 
	    $unwrap_type:ident)),+) => (
		impl Val {
			///Returns the name of this value's primitive type, such as `"nil"` or `"fn"`.
			pub fn type_name(&self) -> &'static str {
				match *self {
					Val::Nil => "nil",
					$(Val::$variant(_) => $type_name),+
				}
			}

			/**
			Returns the name of this value's primitive type, prefixed with the indefinite article,
			such as `"an arr"` or `"a fn"`.

				match val {
					Val::Int(_) => (),
					_ => bail!("expected an int, received {}", val.a_type_name())
				}
			*/
			pub fn a_type_name(&self) -> &'static str {
				match *self {
					Val::Nil => "a nil",
					$(Val::$variant(_) => $a_type_name),+
				}
			}
		}

		impl Val {
			$(
				#[inline]
				pub fn $is_type(&self) -> bool {
					match *self {
						Val::$variant(_) => true,
						_ => false
					}
				}

				#[inline]
				pub fn $unwrap_type(self) -> $type {
					match self {
						Val::$variant(inner) => inner,
						_ => panic!("attempted to unwrap {} Val as {}", self.a_type_name(),
						            $a_type_name)

					}
				}
			)+
		}
	);
}

impl_val!(
	(Int, i32, "int", "an int", is_int, unwrap_int), 
	(Flo, f32, "flo", "a flo", is_flo, unwrap_flo), 
	(Char, char, "char", "a char", is_char, unwrap_char), 
	(Bool, bool, "bool", "a bool", is_bool, unwrap_bool), 
	(Sym, Sym, "sym", "a sym", is_sym, unwrap_sym), 
	(RFn, RFn, "rfn", "an rfn", is_rfn, unwrap_rfn),
	(Arr, Root<Arr>, "arr", "an arr", is_arr, unwrap_arr),
	(Str, Root<Str>, "str", "a str", is_str, unwrap_str),
	(Tab, Root<Tab>, "tab", "a tab", is_tab, unwrap_tab),
	(GIter, Root<GIter>, "iter", "an iter", is_giter, unwrap_giter),
	(Obj, Root<Obj>, "obj", "a obj", is_obj, unwrap_obj),
	(Class, Root<Class>, "class", "a class", is_class, unwrap_class),
	(GFn, Root<GFn>, "fn", "a fn", is_gfn, unwrap_gfn),
	(Coro, Root<Coro>, "coro", "a coro", is_coro, unwrap_coro),
	(RData, Root<RData>, "rdata", "an rdata", is_rdata, unwrap_rdata)
);

impl Val {
	pub fn is_nil(&self) -> bool {
		match *self {
			Val::Nil => true,
			_ => false
		}
	}

	///Returns `true` if the value is anything other than `#n` or `#f`.
	pub fn is_truthy(&self) -> bool {
		match *self {
			Val::Nil | Val::Bool(false) => false,
			_ => true
		}
	}

	///Returns `true` if the value is `#n` or `#f`.
	pub fn is_falsy(&self) -> bool {
		match *self {
			Val::Nil | Val::Bool(false) => true,
			_ => false
		}
	}

	//if you change any of these, you also need to change the OpPredicate instruction in vm.rs...

	/**
	Returns `true` if the value belongs to the `num` abstract type (`int` or `flo`).
	*/
	pub fn is_num(&self) -> bool {
		matches!(*self, Val::Int(_) | Val::Flo(_))
	}

	/**
	Returns `true` if the value belongs to the `deque` abstract type (`arr` or `str`).
	*/
	pub fn is_deque(&self) -> bool {
		matches!(*self, Val::Arr(_) | Val::Str(_))
	}

	/**
	Returns `true` if the value belongs to the `callable` abstract type (`fn`, `rfn` or `class`).
	*/
	pub fn is_callable(&self) -> bool {
		matches!(*self, Val::GFn(_) | Val::RFn(_) | Val::Class(_))
	}
	
	/**
	Returns `true` if the value belongs to the `expander` abstract type (`fn` or `rfn`).
	*/
	pub fn is_expander(&self) -> bool {
		matches!(*self, Val::GFn(_) | Val::RFn(_))
	}

	/**
	Returns `true` if the value belongs to the `iterable` abstract type (`arr`, `str`, `tab`,
	`iter` or `coro`).
	*/
	pub fn is_iterable(&self) -> bool {
		matches!(*self, Val::Arr(_) | Val::Str(_) | Val::Tab(_) | Val::GIter(_) | Val::Coro(_))
	}

	/**
	Creates a shallow copy of the value.

	Equivalent to [`(clone val)`](https://gamelisp.rs/std/clone).
	*/
	pub fn shallow_clone(&self) -> GResult<Val> {
		Ok(match *self {
			Val::Nil => Val::Nil,
			Val::Int(i) => Val::Int(i),
			Val::Flo(f) => Val::Flo(f),
			Val::Char(c) => Val::Char(c),
			Val::Bool(b) => Val::Bool(b),
			Val::Sym(s) => Val::Sym(s),
			Val::RFn(r) => Val::RFn(r),
			Val::Arr(ref arr) => Val::Arr(arr.shallow_clone()),
			Val::Str(ref st) => Val::Str(st.shallow_clone()),
			Val::Tab(ref tab) => Val::Tab(tab.shallow_clone()),
			Val::GIter(ref giter) => Val::GIter(giter.shallow_clone()),
			Val::Obj(ref root) => {
				let val: Option<Val> = root.call_if_present(OP_CLONE_SYM, &())?;
				match val {
					Some(val) => val,
					None => Val::Obj(root.clone())
				}
			}
			Val::Class(ref root) => Val::Class(root.clone()),
			Val::GFn(ref root) => Val::GFn(root.clone()),
			Val::Coro(ref root) => Val::Coro(root.clone()),
			Val::RData(ref root) => {
				let val: Option<Val> = root.call_if_present(OP_CLONE_SYM, &())?;
				match val {
					Some(val) => val,
					None => Val::RData(root.clone())
				}
			}
		})
	}

	/**
	Recursively copies the value and all of its contents.

	Equivalent to [`(deep-clone val)`](https://gamelisp.rs/std/deep-clone).
	*/

	//todo: check for reference cycles
	pub fn deep_clone(&self) -> GResult<Val> {
		Ok(match *self {
			Val::Nil => Val::Nil,
			Val::Int(i) => Val::Int(i),
			Val::Flo(f) => Val::Flo(f),
			Val::Char(c) => Val::Char(c),
			Val::Bool(b) => Val::Bool(b),
			Val::Sym(s) => Val::Sym(s),
			Val::RFn(r) => Val::RFn(r),
			Val::Arr(ref arr) => Val::Arr(arr.deep_clone()?),
			Val::Str(ref st) => Val::Str(st.shallow_clone()),
			Val::Tab(ref tab) => Val::Tab(tab.deep_clone()?),
			Val::GIter(ref giter) => Val::GIter(giter.shallow_clone()),
			Val::Obj(ref root) => {
				let mut val: Option<Val> = root.call_if_present(OP_DEEP_CLONE_SYM, &())?;
				if val.is_none() {
					val = root.call_if_present(OP_CLONE_SYM, &())?;
				}

				match val {
					Some(val) => val,
					None => Val::Obj(root.clone())
				}
			}
			Val::Class(ref root) => Val::Class(root.clone()),
			Val::GFn(ref root) => Val::GFn(root.clone()),
			Val::Coro(ref root) => Val::Coro(root.clone()),
			Val::RData(ref root) => {
				let mut val: Option<Val> = root.call_if_present(OP_DEEP_CLONE_SYM, &())?;
				if val.is_none() {
					val = root.call_if_present(OP_CLONE_SYM, &())?;
				}

				match val {
					Some(val) => val,
					None => Val::RData(root.clone())
				}
			}
		})
	}

	/**
	Makes the value immutable.

	Equivalent to [`(freeze! val)`](https://gamelisp.rs/std/freeze-mut).
	*/
	pub fn freeze(&self) {
		match *self {
			Val::Arr(ref arr) => arr.freeze(),
			Val::Str(ref st) => st.freeze(),
			Val::Tab(ref tab) => tab.freeze(),
			Val::Obj(ref obj) => obj.freeze(),
			Val::Nil | Val::Int(_) | Val::Flo(_) | Val::Char(_) | Val::Bool(_) | Val::Sym(_) |
			Val::GIter(_) | Val::RFn(_) | Val::Class(_) | Val::GFn(_) | Val::Coro(_) | Val::RData(_) => ()
		}
	}

	/**
	Makes the value and all of its contents immutable.

	Equivalent to [`(deep-freeze! val)`](https://gamelisp.rs/std/deep-freeze-mut).
	*/

	//todo: handle reference cycles
	pub fn deep_freeze(&self) {
		match *self {
			Val::Arr(ref arr) => arr.deep_freeze(),
			Val::Tab(ref tab) => tab.deep_freeze(),
			Val::Str(ref st) => st.freeze(),
			Val::Obj(ref obj) => {
				//todo
				obj.freeze()
			}
			Val::Nil | Val::Int(_) | Val::Flo(_) | Val::Char(_) | Val::Bool(_) | Val::Sym(_) |
			Val::GIter(_) | Val::RFn(_) | Val::Class(_) | Val::GFn(_) | Val::Coro(_) | Val::RData(_) => ()
		}
	}

	//todo: handle reference cycles
	pub(crate) fn is_deep_frozen(&self) -> bool {
		//note that there's currently no way to traverse the fields of objects etc., so it only
		//makes sense to use this method for representable/evaluable values
		match *self {
			Val::Arr(ref arr) => arr.is_deep_frozen(),
			Val::Str(ref st) => st.is_frozen(),
			Val::Tab(ref tab) => tab.is_deep_frozen(),

			Val::Nil | Val::Int(_) | Val::Flo(_) | 
			Val::Char(_) | Val::Bool(_) | Val::Sym(_) => true,

			Val::Obj(_) | Val::RFn(_) | Val::Class(_) | Val::GIter(_) | 
			Val::GFn(_) | Val::Coro(_) | Val::RData(_) => unreachable!()
		}
	}
}


//-------------------------------------------------------------------------------------------------
// Num
//-------------------------------------------------------------------------------------------------

/**
A type-erased `num`.

In general, you can manipulate a `Num` in the same way that you would manipulate an `i32`
or `f32`. `Num` supports familiar methods like [`abs`](#method.abs) and 
[`div_euclid`](#method.div_euclid), as well as the built-in numeric operators like
`+` and `%`.
*/

#[derive(Clone, Copy)]
pub enum Num {
	Int(i32),
	Flo(f32)
}

//todo: significantly more methods, to bring Num on-par with the built-in numeric types where
//possible, so that it can be used transparently in FFI code. could define simple forwarding
//methods, available on both f32 and i32, using a macro.

impl Num {
	pub fn is_int(self) -> bool { 
		if let Num::Int(_) = self { true } else { false } 
	}

	pub fn is_flo(self) -> bool { 
		if let Num::Flo(_) = self { true } else { false } 
	}

	pub fn unwrap_int(self) -> i32 { 
		if let Num::Int(i) = self { i } else { panic!() } 
	}

	pub fn unwrap_flo(self) -> f32 { 
		if let Num::Flo(f) = self { f } else { panic!() } 
	}
	
	pub fn into_f32(self) -> f32 {
		match self {
			Num::Int(i) => i as f32,
			Num::Flo(f) => f
		}
	}

	pub fn abs(self) -> Num {
		match self {
			Num::Int(i) => Num::Int(i.wrapping_abs()),
			Num::Flo(f) => Num::Flo(f.abs())
		}
	}

	pub fn div_euclid(self, other: Num) -> Num {
		match (self, other) {
			(Num::Int(left), Num::Int(right)) => Num::Int(left.div_euclid(right)),
			(Num::Flo(left), Num::Int(right)) => Num::Flo(left.div_euclid(right as f32)),
			(Num::Int(left), Num::Flo(right)) => Num::Flo((left as f32).div_euclid(right)),
			(Num::Flo(left), Num::Flo(right)) => Num::Flo(left.div_euclid(right))
		}
	}

	pub fn wrapping_div_euclid(self, other: Num) -> Num {
		match (self, other) {
			(Num::Int(left), Num::Int(right)) => Num::Int(left.wrapping_div_euclid(right)),
			(Num::Flo(left), Num::Int(right)) => Num::Flo(left.div_euclid(right as f32)),
			(Num::Int(left), Num::Flo(right)) => Num::Flo((left as f32).div_euclid(right)),
			(Num::Flo(left), Num::Flo(right)) => Num::Flo(left.div_euclid(right))
		}
	}

	pub fn rem_euclid(self, other: Num) -> Num {
		match (self, other) {
			(Num::Int(left), Num::Int(right)) => Num::Int(left.rem_euclid(right)),
			(Num::Flo(left), Num::Int(right)) => Num::Flo(left.rem_euclid(right as f32)),
			(Num::Int(left), Num::Flo(right)) => Num::Flo((left as f32).rem_euclid(right)),
			(Num::Flo(left), Num::Flo(right)) => Num::Flo(left.rem_euclid(right))
		}
	}

	pub fn wrapping_rem_euclid(self, other: Num) -> Num {
		match (self, other) {
			(Num::Int(left), Num::Int(right)) => Num::Int(left.wrapping_rem_euclid(right)),
			(Num::Flo(left), Num::Int(right)) => Num::Flo(left.rem_euclid(right as f32)),
			(Num::Int(left), Num::Flo(right)) => Num::Flo((left as f32).rem_euclid(right)),
			(Num::Flo(left), Num::Flo(right)) => Num::Flo(left.rem_euclid(right))
		}
	}
}

impl Default for Num {
	fn default() -> Num {
		Num::Int(0)
	}
}

impl Debug for Num {
	fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
		match *self {
			Num::Int(ref i) => Debug::fmt(i, formatter),
			Num::Flo(ref f) => Debug::fmt(f, formatter),
		}
	}
}

impl Display for Num {
	fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
		match *self {
			Num::Int(ref i) => Display::fmt(i, formatter),
			Num::Flo(ref f) => Display::fmt(f, formatter),
		}
	}
}

impl PartialEq<Num> for Num {
	fn eq(&self, other: &Num) -> bool {
		match (*self, *other) {
			(Num::Int(i0), Num::Int(i1)) => i0 == i1,
			(Num::Int(i0), Num::Flo(f1)) => i0 as f32 == f1,
			(Num::Flo(f0), Num::Int(i1)) => f0 == i1 as f32,
			(Num::Flo(f0), Num::Flo(f1)) => f0 == f1
		}
	}
}

impl PartialEq<i32> for Num {
	fn eq(&self, other: &i32) -> bool {
		self.eq(&Num::Int(*other))
	}
}

impl PartialEq<Num> for i32 {
	fn eq(&self, other: &Num) -> bool {
		Num::Int(*self).eq(other)
	}
}

impl PartialEq<f32> for Num {
	fn eq(&self, other: &f32) -> bool {
		self.eq(&Num::Flo(*other))
	}
}

impl PartialEq<Num> for f32 {
	fn eq(&self, other: &Num) -> bool {
		Num::Flo(*self).eq(other)
	}
}

impl PartialOrd<Num> for Num {
	fn partial_cmp(&self, other: &Num) -> Option<Ordering> {
		match (*self, *other) {
			(Num::Int(i0), Num::Int(i1)) => i0.partial_cmp(&i1),
			(Num::Int(i0), Num::Flo(f1)) => (i0 as f32).partial_cmp(&f1),
			(Num::Flo(f0), Num::Int(i1)) => f0.partial_cmp(&(i1 as f32)),
			(Num::Flo(f0), Num::Flo(f1)) => f0.partial_cmp(&f1)
		}
	}
}

impl PartialOrd<i32> for Num {
	fn partial_cmp(&self, other: &i32) -> Option<Ordering> {
		self.partial_cmp(&Num::Int(*other))
	}
}

impl PartialOrd<Num> for i32 {
	fn partial_cmp(&self, other: &Num) -> Option<Ordering> {
		Num::Int(*self).partial_cmp(other)
	}
}

impl PartialOrd<f32> for Num {
	fn partial_cmp(&self, other: &f32) -> Option<Ordering> {
		self.partial_cmp(&Num::Flo(*other))
	}
}

impl PartialOrd<Num> for f32 {
	fn partial_cmp(&self, other: &Num) -> Option<Ordering> {
		Num::Flo(*self).partial_cmp(other)
	}
}

impl Add for Num {
	type Output = Num;
	fn add(self, rhs: Num) -> Num {
		match (self, rhs) {
			(Num::Int(i0), Num::Int(i1)) => Num::Int(i0.wrapping_add(i1)),
			(Num::Int(i0), Num::Flo(f1)) => Num::Flo(i0 as f32 + f1),
			(Num::Flo(f0), Num::Int(i1)) => Num::Flo(f0 + i1 as f32),
			(Num::Flo(f0), Num::Flo(f1)) => Num::Flo(f0 + f1)
		}
	}
}

impl Sub for Num {
	type Output = Num;
	fn sub(self, rhs: Num) -> Num {
		match (self, rhs) {
			(Num::Int(i0), Num::Int(i1)) => Num::Int(i0.wrapping_sub(i1)),
			(Num::Int(i0), Num::Flo(f1)) => Num::Flo(i0 as f32 - f1),
			(Num::Flo(f0), Num::Int(i1)) => Num::Flo(f0 - i1 as f32),
			(Num::Flo(f0), Num::Flo(f1)) => Num::Flo(f0 - f1)
		}
	}
}

impl Mul for Num {
	type Output = Num;
	fn mul(self, rhs: Num) -> Num {
		match (self, rhs) {
			(Num::Int(i0), Num::Int(i1)) => Num::Int(i0.wrapping_mul(i1)),
			(Num::Int(i0), Num::Flo(f1)) => Num::Flo(i0 as f32 * f1),
			(Num::Flo(f0), Num::Int(i1)) => Num::Flo(f0 * i1 as f32),
			(Num::Flo(f0), Num::Flo(f1)) => Num::Flo(f0 * f1)
		}
	}
}

impl Div for Num {
	type Output = Num;
	fn div(self, rhs: Num) -> Num {
		match (self, rhs) {
			(Num::Int(i0), Num::Int(i1)) => Num::Int(i0.wrapping_div(i1)),
			(Num::Int(i0), Num::Flo(f1)) => Num::Flo(i0 as f32 / f1),
			(Num::Flo(f0), Num::Int(i1)) => Num::Flo(f0 / i1 as f32),
			(Num::Flo(f0), Num::Flo(f1)) => Num::Flo(f0 / f1)
		}
	}
}

impl Rem for Num {
	type Output = Num;
	fn rem(self, rhs: Num) -> Num {
		match (self, rhs) {
			(Num::Int(i0), Num::Int(i1)) => Num::Int(i0.wrapping_rem(i1)),
			(Num::Int(i0), Num::Flo(f1)) => Num::Flo(i0 as f32 % f1),
			(Num::Flo(f0), Num::Int(i1)) => Num::Flo(f0 % i1 as f32),
			(Num::Flo(f0), Num::Flo(f1)) => Num::Flo(f0 % f1)
		}
	}
}

impl Neg for Num {
	type Output = Num;
	fn neg(self) -> Num {
		match self {
			Num::Int(i) => Num::Int(i.wrapping_neg()),
			Num::Flo(f) => Num::Flo(-f)
		}
	}
}


//-------------------------------------------------------------------------------------------------
// Val equality and hashing
//-------------------------------------------------------------------------------------------------

//todo: support equality for cyclic data structures. racket's rule is that "two objects with
//cycles are equal if their infinite unfoldings would be equal to one another" - makes sense.
//consider generalizing cycle detection and using it for deep-freeze! and deep-clone as well.

impl Val {

	///Equivalent to [`(== self other)`](https://gamelisp.rs/std/num-eq).
	#[inline]
	pub fn num_eq(&self, other: &Val) -> Option<bool> {
		match (self, other) {
			(&Val::Int(i0), &Val::Int(i1)) => Some(i0 == i1),
			(&Val::Flo(f0), &Val::Int(i1)) => Some(f0 == i1 as f32),
			(&Val::Char(c0), &Val::Int(i1)) => Some(c0 as u32 as i32 == i1),
			(&Val::Int(i0), &Val::Flo(f1)) => Some(i0 as f32 == f1),
			(&Val::Flo(f0), &Val::Flo(f1)) => Some(f0 == f1),
			(&Val::Char(c0), &Val::Flo(f1)) => Some(c0 as u32 as f32 == f1),
			(&Val::Int(i0), &Val::Char(c1)) => Some(i0 == c1 as u32 as i32),
			(&Val::Flo(f0), &Val::Char(c1)) => Some(f0 == c1 as u32 as f32),
			(&Val::Char(c0), &Val::Char(c1)) => Some(c0 == c1),
			_ => None /*bail!("attempted to compare {} and {} using =", 
			           self.a_type_name(), other.a_type_name())*/
		}
	}

	///Equivalent to [`(same? self other)`](https://gamelisp.rs/std/same-p).
	pub fn same(&self, other: &Val) -> bool {
		match (self, other) {
			(&Val::Nil, &Val::Nil) => true,
			(&Val::Int(_), &Val::Int(_)) => self.num_eq(other).unwrap(),
			(&Val::Int(_), &Val::Flo(_)) => self.num_eq(other).unwrap(),
			(&Val::Int(_), &Val::Char(_)) => self.num_eq(other).unwrap(),
			(&Val::Flo(_), &Val::Int(_)) => self.num_eq(other).unwrap(),
			(&Val::Flo(_), &Val::Flo(_)) => self.num_eq(other).unwrap(),
			(&Val::Flo(_), &Val::Char(_)) => self.num_eq(other).unwrap(),
			(&Val::Char(_), &Val::Int(_)) => self.num_eq(other).unwrap(),
			(&Val::Char(_), &Val::Flo(_)) => self.num_eq(other).unwrap(),
			(&Val::Char(_), &Val::Char(_)) => self.num_eq(other).unwrap(),
			(&Val::Bool(b0), &Val::Bool(b1)) => b0 == b1,
			(&Val::Sym(s0), &Val::Sym(s1)) => s0 == s1,
			(&Val::RFn(f0), &Val::RFn(f1)) => f0 == f1,
			(&Val::Arr(ref root0),   &Val::Arr(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::Str(ref root0),   &Val::Str(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::Tab(ref root0),   &Val::Tab(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::GIter(ref root0),   &Val::GIter(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::Obj(ref root0),   &Val::Obj(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::Class(ref root0), &Val::Class(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::GFn(ref root0),   &Val::GFn(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::Coro(ref root0),  &Val::Coro(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::RData(ref root0),   &Val::RData(ref root1)) => Root::ptr_eq(root0, root1),
			_ => false
		}
	}

	///Equivalent to [`(keys-eqv? self other)`](https://gamelisp.rs/std/keys-eqv-p).
	pub fn keys_eqv(&self, other: &Val) -> bool {
		match (self, other) {
			(&Val::Int(_), &Val::Flo(_)) => false,
			(&Val::Int(_), &Val::Char(_)) => false,
			(&Val::Flo(_), &Val::Int(_)) => false,
			(&Val::Flo(_), &Val::Char(_)) => false,
			(&Val::Char(_), &Val::Int(_)) => false,
			(&Val::Char(_), &Val::Flo(_)) => false,
			(&Val::Flo(f0), &Val::Flo(f1)) => {
				match (f0.classify(), f1.classify()) {
					(FpCategory::Nan, FpCategory::Nan) => true,
					_ => f0 == f1
				}
			}
			(&Val::Tab(ref root0), &Val::Tab(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::Obj(ref root0), &Val::Obj(ref root1)) => Root::ptr_eq(root0, root1),
			(&Val::RData(ref root0), &Val::RData(ref root1)) => Root::ptr_eq(root0, root1),
			_ => self.eq(other)
		}
	}

	//this isn't exposed to the user, but it's the test for deciding whether two literals can be
	//merged into one. it's currently the same as `eq?` for tables; it special-cases 0.0 and -0.0
	//to be different from one another; and it's the same as `keys-eqv?` for everything else.
	pub(crate) fn literal_eq(&self, other: &Val) -> bool {
		match (self, other) {
			(&Val::Tab(_), &Val::Tab(_)) => self.eq(other),
			(&Val::Flo(f0), &Val::Flo(f1)) if f0 == 0.0 && f1 == 0.0 => false,
			_ => self.keys_eqv(other)
		}
	}

	/**
	Equivalent to [`(eq? self other)`](https://gamelisp.rs/std/eq-p).

	Note that, because this method may need to invoke an `op-eq?` method when both of its
	arguments are objects or `RData`, it can potentially fail.

	The same is true for `PartialEq` comparisons between values using Rust's `==` operator.
	In that case, if an error occurs, the operator will panic.
	*/

	pub fn try_eq(&self, other: &Val) -> GResult<bool> {
		match (self, other) {
			(&Val::Arr(ref a0), &Val::Arr(ref a1)) => a0.try_eq(a1),
			(&Val::Str(ref s0), &Val::Str(ref s1)) => Ok(**s0 == **s1),
			(&Val::Tab(ref t0), &Val::Tab(ref t1)) => t0.try_eq(t1),
			(&Val::Obj(ref o0), &Val::Obj(ref o1)) => o0.try_eq(o1),
			(&Val::RData(ref r0), &Val::RData(ref r1)) => r0.try_eq(r1),
			_ => Ok(self.same(other))
		}
	}
}

//`val0 == val` has the same semantics as `eq?`. for symmetry with the other equality methods,
//it can be called as val0.eq(&val1). errors in `op-eq?` will panic - try_eq() is the alternative.
impl PartialEq<Val> for Val {
	fn eq(&self, other: &Val) -> bool {
		match (self, other) {
			(&Val::Arr(_), &Val::Arr(_)) => self.try_eq(other).unwrap(),
			(&Val::Str(_), &Val::Str(_)) => self.try_eq(other).unwrap(),
			(&Val::Tab(_), &Val::Tab(_)) => self.try_eq(other).unwrap(),
			(&Val::Obj(_), &Val::Obj(_)) => self.try_eq(other).unwrap(),
			(&Val::RData(_), &Val::RData(_)) => self.try_eq(other).unwrap(),
			_ => self.same(other)
		}
	}
}

impl Val {
	#[doc(hidden)]
	//todo: how to handle (ord nan 1), etc? currently panics.
	pub fn num_cmp(&self, other: &Val) -> Option<Ordering> {
		match (self, other) {
			(&Val::Int(i0), &Val::Int(i1)) => Some(i0.cmp(&i1)),
			(&Val::Flo(f0), &Val::Int(i1)) => f0.partial_cmp(&(i1 as f32)),
			(&Val::Int(i0), &Val::Flo(f1)) => (i0 as f32).partial_cmp(&f1),
			(&Val::Flo(f0), &Val::Flo(f1)) => f0.partial_cmp(&f1),

			(&Val::Char(c0), &Val::Int(i1)) => Some((c0 as u32 as i32).cmp(&i1)),
			(&Val::Char(c0), &Val::Flo(f1)) => (c0 as u32 as f32).partial_cmp(&f1),
			(&Val::Int(i0), &Val::Char(c1)) => Some(i0.cmp(&(c1 as u32 as i32))),
			(&Val::Flo(f0), &Val::Char(c1)) => f0.partial_cmp(&(c1 as u32 as f32)),

			(&Val::Char(c0), &Val::Char(c1)) => {
				Some((c0 as u32 as i32).cmp(&(c1 as u32 as i32)))
			}

			_ => None
		}
	}
}

macro_rules! partial_cmp_method (
	($num_name:ident, $name:ident, $return_type:ty) => (
		impl Val {
			#[doc(hidden)]
			pub fn $num_name(&self, other: &Val) -> Option<$return_type> {
				match (self, other) {
					(&Val::Int(i0), &Val::Int(i1)) => Some(i0.$name(&i1)),
					(&Val::Flo(f0), &Val::Int(i1)) => Some(f0.$name(&(i1 as f32))),
					(&Val::Int(i0), &Val::Flo(f1)) => Some((i0 as f32).$name(&f1)),
					(&Val::Flo(f0), &Val::Flo(f1)) => Some(f0.$name(&f1)),

					(&Val::Char(c0), &Val::Int(i1)) => Some((c0 as u32 as i32).$name(&i1)),
					(&Val::Char(c0), &Val::Flo(f1)) => Some((c0 as u32 as f32).$name(&f1)),
					(&Val::Int(i0), &Val::Char(c1)) => Some(i0.$name(&(c1 as u32 as i32))),
					(&Val::Flo(f0), &Val::Char(c1)) => Some(f0.$name(&(c1 as u32 as f32))),

					(&Val::Char(c0), &Val::Char(c1)) => {
						Some((c0 as u32 as i32).$name(&(c1 as u32 as i32)))
					}

					_ => None
				}
			}
		}
	);
);

partial_cmp_method!(num_lt, lt, bool);
partial_cmp_method!(num_le, le, bool);
partial_cmp_method!(num_gt, gt, bool);
partial_cmp_method!(num_ge, ge, bool);

/**
A thin wrapper over `Val` which enables it to be used as a key in a `HashTable`.

It has an [`Eq`](https://doc.rust-lang.org/core/cmp/trait.Eq.html) implementation which calls 
[`keys_eqv`](enum.Val.html#method.keys_eqv) rather than [`eq`](enum.Val.html#method.try_eq).
*/

//this seems limited and insufficiently general (e.g. can't use a Root<Obj> as a HashMap key), 
//so i'm going to hide it for now
#[doc(hidden)]
#[derive(Clone)]
pub struct Hashable(pub Val);

impl PartialEq<Hashable> for Hashable {
	fn eq(&self, other: &Hashable) -> bool {
		self.0.keys_eqv(&other.0)
	}
}

impl Eq for Hashable { }

//if you change this implementation, you must also change the implementation for Slot
impl Hash for Hashable {
	fn hash<H: Hasher>(&self, state: &mut H) {
		//the documentation for Hash states that if two values implement Eq and compare equal to
		//one another, they must generate the same hash value. this means that our hash() method
		//must deeply inspect arrs, strs and tabs, in the same way that keys_eqv does.
		match self.0 {
			Val::Nil => 0u8.hash(state),
			Val::Int(i) => i.hash(state),
			Val::Flo(f) => {
				//if two float values compare equivalent to one another, they must generate the
				//same hash, i.e. they must pass the same bytes to the hasher.
				match f.classify() {
					FpCategory::Zero => 0u8.hash(state),
					FpCategory::Infinite => {
						if f > 0.0 { 1u8.hash(state) } else { 2u8.hash(state) }
					}
					FpCategory::Nan => 3u8.hash(state),
					FpCategory::Normal | FpCategory::Subnormal => {
						//despite what some answers on stackoverflow will tell you, both normal
						//and subnormal floats do not have any redundancy: each possible value has
						//exactly one bit representation. this is easy to prove by iterating
						//over every subnormal float (bit representations from 1 to 2^23-1) and
						//asserting that compared to its predecessor, it's always > and !=.
						//also checked the decimal string representation of each subnormal float, 
						//and confirmed that there are as many different strings as there are 
						//subnormal float values.
						f.to_bits().hash(state)
					}
				}
			}
			Val::Bool(b) => b.hash(state),
			Val::Char(c) => c.hash(state),
			Val::Sym(s) => s.hash(state),
			Val::RFn(f) => f.hash(state),
			Val::Arr(ref arr) => (**arr).hash(state),
			Val::Str(ref st) => (**st).hash(state),
			Val::Tab(ref root) => (&**root as *const _ as usize).hash(state),
			Val::GIter(ref root) => (&**root as *const _ as usize).hash(state),
			Val::Obj(ref root) => (&**root as *const _ as usize).hash(state),
			Val::Class(ref root) => (&**root as *const _ as usize).hash(state),
			Val::GFn(ref root) => (&**root as *const _ as usize).hash(state),
			Val::Coro(ref root) => (&**root as *const _ as usize).hash(state),
			Val::RData(ref root) => (&**root as *const _ as usize).hash(state)
		}
	}
}
