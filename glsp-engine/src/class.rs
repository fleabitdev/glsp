use fnv::{FnvHashMap, FnvHashSet};
use smallvec::{SmallVec};
use super::code::{GFn};
use super::collections::{Arr, DequeAccess, DequeOps, Tab};
use super::engine::{glsp, Guard, Sym, stock_syms::*, ToSym, with_heap, with_vm};
use super::error::{GResult};
use super::gc::{Allocate, Gc, GcHeader, Slot, Root, Visitor};
use super::iter::{GIter, GIterState};
use super::val::{Val};
use super::wrap::{CallableOps, FromVal, ToCallArgs, ToVal};
use std::{u16, str};
use std::cell::{RefCell, RefMut};
use std::cmp::{Ord};
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::{FromIterator};
use std::mem::{forget, size_of};

/*

the `class` macro converts its input into a raw-class tab (which is a fairly straight translation
of its fields, except that the state hierarchy is flattened and any toplevel items are bundled into
a `Main` state). it expands to a call to (%make-class the-raw-class-tab). any method bodies and 
field/const initializers receive some light preprocessing for @ forms, etc., and then emit a
(fn) form which is passed to the next stages as a Gc<GFn>.

make-class converts its input raw-class tab into a RawClass struct (again, pretty much a one-to-one
translation). for each mixin, that mixin's RawClass is applied to the target RawClass, mutating it.

finally, a Class is built from the mixed RawClass. this stage involves invoking any const
initializer GFns, and coalescing name bindings into a "stacked" format when they can shadow or 
wrap one another.

*/

/**
The `obj` primitive type.

Note that [`CallableOps`](trait.CallableOps.html) is implemented for `Root<Class>`, so an
`Obj` can be constructed from a [`Class`](struct.Class.html) using [`glsp::call`](fn.call.html).
*/

pub struct Obj {
	header: GcHeader,
	class: Gc<Class>,
	storage: RefCell<Option<ObjStorage>> //None for a killed obj
}

struct ObjStorage {
	//there is a one-to-one relationship between (field ...) or (prop ...) declarations, and Slots 
	//in this vec. while a state is disabled, all of its fields are set to Slot::Nil.
	fields: Vec<Slot>,

	//this is a bitmask. the lowest bit represents whether state 0 (Main) is enabled, the next
	//bit is for state 1, etc. we could replace this with a SmallBitVec if we decide to support
	//more than 31 state forms per class.
	states_enabled: u32,

	//we need this field to generate a `self` argument when obj.call() is invoked from rust code
	gc_self: Gc<Obj>
}

/**
The `class` primitive type.

Note that [`CallableOps`](trait.CallableOps.html) is implemented for `Root<Class>`, so an
[`Obj`](struct.Obj.html) can be constructed from a `Class` using 
[`glsp::call`](fn.call.html).
*/

pub struct Class {
	header: GcHeader,
	name: Option<Sym>,
	is_mixin: bool,

	//when the user attempts to access a field, const or method from outside the class, it's 
	//routed through this HashMap. it contains bindings for both qualified and unqualified names.
	bindings: FnvHashMap<Sym, Binding>,

	//the number of fields to be allocated by Objs of this class
	field_count: usize,

	//see Binding::StackableField, below
	field_stack: Vec<FieldStackEntry>,

	//see MethBinding::Stackable, below
	meth_stack: Vec<MethStackEntry>,

	//we store references to the class' mixins in no particular order, since we only use this 
	//field to implement the (is?) function.
	is: FnvHashSet<Gc<Class>>,

	//likewise, we can store states in no particular order, since all of the state-priority 
	//information is available in `State`, `field_stack` and `meth_stack`.
	states: FnvHashMap<Sym, State>,

	//see above
	raw_class: Option<Box<RawClass>>
}

//states are an illusion! classes are actually just a flat collection of bindings, where each
//binding can be switched on or off using a particular bit in the Obj. the only information we
//store for each state is its hierarchial relationship with other states (for enab/disab purposes), 
//bindings for its initialization and finalization methods (if any), and the index of its flag bit
#[derive(Clone)]
struct State {

	//flag bit; Main always gets bit 0
	index: u8,

	//relationship to other states
	name: Sym,
	enabled_by_default: bool,
	fsm_siblings: Vec<Sym>,
	parent: Option<Sym>,
	children: Vec<Sym>,

	//bitmasks
	requires: u32,
	required_by: u32,
	excludes: u32,

	//anonymous method bindings
	init: Option<MethBinding>, //the init/init-mixin stack for Main, or init-state otherwise
	finis: Vec<MethBinding>, //the fini/fini-mixin stack for Main, or fini-state otherwise
}

#[derive(Clone)]
enum Binding {
	//a field which does not participate in stacking. the u8 is the index of this field's state; 
	//the field is not considered to exist if the corresponding bit in `states_enabled` is unset.
	//the u16 is the index of the field within the ObjStorage's `fields` vec.
	SimpleField(u8, u16),

	//very similar to SimpleField, but as the value is always immutable, we store it inline
	//in the class rather than wasting space in the ObjStorage.
	SimpleConst(u8, Slot),

	//a field which may be stacked (i.e., has a (field ...) or (const ...) declaration in multiple 
	//states). the u16 is a starting index in the State's `field_stack` Vec. to access the binding,
	//we start at that index and iterate upwards until we encounter one of the following:
	// - a ConstEntry or FieldEntry *for a currently-enabled state*, in which case we handle it
	//   like a SimpleField or SimpleConst
	// - an EndEntry, in which case the field does not currently exist
	StackableField(u16),

	//a method, which may be simple or stacked. we offload the complexity here to MethBinding, so 
	//that it can be shared with properties, init and fini
	Meth(MethBinding),

	//a property, which may be simple or stacked. the first Option is the getter, the second is 
	//the setter.
	Prop(Option<MethBinding>, Option<MethBinding>),
}

#[derive(Clone)]
enum MethBinding {
	//a method which does not participate in stacking. the u8 is the state index. the GFn can
	//be called directly, passing in a `self` Slot::Obj as its first parameter, and a
	//`next_base_index` (int or nil) as the second parameter if the bool flag is true.
	Simple(u8, Gc<GFn>, bool),

	//a method which may be stacked. works similar to StackableField, except that the only two
	//options are "end of stack" or "method binding"
	//
	//the u8 is the state of the qualified binding, so that if the user accesses the meth
	//'Foo:x while 'Foo is disabled, it will fail rather than propagating to a wrapped method. 
	//for unqualified bindings, the u8 is 0 (i.e. the Main state).
	Stackable(u8, u16),
}

//see Binding::StackableField, above
#[derive(Clone)]
enum FieldStackEntry {
	Field(u8, u16),
	Const(u8, Slot),
	End
}

//see MethBinding::Stackable, above
#[derive(Clone)]
enum MethStackEntry {
	Meth(u8, Gc<GFn>, bool),
	End
}

impl Binding {
	fn to_field_stack_entry(&self) -> FieldStackEntry {
		match *self {
			Binding::SimpleField(state_i, field_i) => {
				FieldStackEntry::Field(state_i, field_i)
			}
			Binding::SimpleConst(state_i, ref slot) => {
				FieldStackEntry::Const(state_i, slot.clone())
			}
			_ => unreachable!()
		}
	}

	fn to_meth_stack_entry(&self) -> MethStackEntry {
		match *self {
			Binding::Meth(ref meth_binding) => meth_binding.to_meth_stack_entry(),
			_ => unreachable!()
		}
	}
}

impl MethBinding {
	fn to_meth_stack_entry(&self) -> MethStackEntry {
		match *self {
			MethBinding::Simple(state_i, ref gfn, rni) => {
				MethStackEntry::Meth(state_i, gfn.clone(), rni)
			}
			_ => unreachable!()
		}
	}
}


//-------------------------------------------------------------------------------------------------
// public methods
//-------------------------------------------------------------------------------------------------

impl Class {
	pub(crate) fn new(tab: &Tab) -> GResult<Class> {
		let mut raw_class = RawClass::from_tab(tab)?;

		//for mixins, we return a fake/empty class which just wraps the RawClass. (this means
		//that mixins aren't properly checked for validity until they've already been mixed,
		//but all that we're really missing out on is const evaluation and method stacking,
		//neither of which can actually be checked for an unmixed mixin!)
		if raw_class.is_mixin {
			Ok(Class {
				header: GcHeader::new(),
				name: raw_class.name,
				is_mixin: true,

				bindings: FnvHashMap::default(),
				field_count: 0,
				field_stack: Vec::new(),
				meth_stack: Vec::new(),
				is: FnvHashSet::default(),
				states: FnvHashMap::default(),

				raw_class: Some(Box::new(raw_class))
			})
		} else {
			raw_class.mix()?;
			ClassBuilder::new(raw_class)?.build()
		}
	}

	#[inline(always)]
	fn lookup(&self, key: Sym) -> Option<Slot> {

		//which fields/constants we should expose on the class is a tricky dilemma. if there's
		//a constant Main:c shadowed by a field State:c, what should we do? for now, we just 
		//pretend that no states exist other than Main. ideally i suppose we should also
		//support fully-qualified references to consts in other states (todo?)

		if let Some(binding) = self.bindings.get(&key) {
			match *binding {
				Binding::SimpleConst(0, ref slot) => Some(slot.clone()),
				Binding::StackableField(mut stack_index) => {
					loop {
						match &self.field_stack[stack_index as usize] {
							FieldStackEntry::Field(0, _) => return None,
							FieldStackEntry::Const(0, ref slot) => return Some(slot.clone()),
							FieldStackEntry::End => return None,
							_ => stack_index += 1
						}
					}
				}
				_ => None
			}
		} else {
			None
		}
	}

	/**
	Accesses the value of a constant.
	
	Equivalent to [`[cls key]`](https://gamelisp.rs/std/access).
	*/
	pub fn get<S: ToSym, V: FromVal>(&self, key: S) -> GResult<V> {
		let sym = key.to_sym()?;

		match self.lookup(sym) {
			Some(slot) => Ok(V::from_slot(&slot)?),
			None => {
				bail!("attempted to access nonexistent const '{}'", sym)
			}
		}
	}

	/**
	Accesses the value of a constant if it's defined.
	
	Equivalent to [`[cls (? key)]`](https://gamelisp.rs/std/access).
	*/
	pub fn get_if_present<S: ToSym, V: FromVal>(&self, key: S) -> GResult<Option<V>> {
		let sym = key.to_sym()?;

		match self.lookup(sym) {
			Some(slot) => Ok(Some(V::from_slot(&slot)?)),
			None => Ok(None)
		}
	}

	//designed to imitate Obj::get_method(). used by vm.rs
	pub(crate) fn get_method(&self, key: Sym) -> Option<(Slot, bool, bool, Slot)> {
		match self.lookup(key) {
			Some(slot @ Slot::GFn(_)) | Some(slot @ Slot::RFn(_)) | Some(slot @ Slot::Class(_)) => {
				Some((slot, false, false, Slot::Nil))
			}
			Some(_) => None,
			None => None
		}
	}

	/**
	Invokes a callable value stored in a constant.
	
	Equivalent to [`(call-meth key cls ..args)`](https://gamelisp.rs/std/call-meth).
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
	If a constant with the given name is defined, and if it stores a callable value,
	invokes it as a function and returns its result. Otherwise, returns `None`.
	
	Equivalent to [`(call-meth (? key) cls ..args)`](https://gamelisp.rs/std/call-meth).
	*/
	pub fn call_if_present<S, A, R>(&self, key: S, args: &A) -> GResult<Option<R>> 
	where
		S: ToSym,
		A: ToCallArgs + ?Sized, 
		R: FromVal
	{
		let sym = key.to_sym()?;

		match self.lookup(sym) {
			Some(Slot::GFn(gfn)) => Ok(Some(glsp::call(&gfn, args)?)),
			Some(Slot::RFn(rfn)) => Ok(Some(glsp::call(&rfn, args)?)),
			Some(Slot::Class(class_to_call)) => Ok(Some(glsp::call(&class_to_call, args)?)),
			Some(_) => Ok(None),
			None => Ok(None)
		}
	}

	/**
	Returns `true` if the given name is bound to a constant with a callable value.

	Equivalent to [`(has-meth? cls key)`](https://gamelisp.rs/std/has-meth-p).
	*/
	pub fn has_meth<S: ToSym>(&self, key: S) -> GResult<bool> {
		let sym = key.to_sym()?;

		match self.lookup(sym) {
			Some(Slot::GFn(_)) => Ok(true),
			Some(Slot::RFn(_)) => Ok(true),
			Some(Slot::Class(_)) => Ok(true),
			Some(_) => Ok(false),
			None => Ok(false)
		}
	}

	/**
	Creates an indexing iterator for this collection.

	Equivalent to [`[cls iter]`](https://gamelisp.rs/std/access).
	*/
	pub fn access_giter(class: &Root<Class>, giter: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::AccessClass(class.to_gc(), giter.to_gc()))
	}

	/**
	Returns `true` if the given name is bound to a constant.

	Equivalent to [`(has? cls key)`](https://gamelisp.rs/std/has-p).
	*/
	pub fn has<S: ToSym>(&self, key: S) -> GResult<bool> {
		let sym = key.to_sym()?;
		Ok(self.lookup(sym).is_some())
	}

	/**
	Returns the name of the class, if any.

	Equivalent to [`(class-name cls)`](https://gamelisp.rs/std/class-name).
	*/
	pub fn name(&self) -> Option<Sym> {
		self.name
	}

	/**
	Returns `true` if the class defines a state with the given name.

	Equivalent to [`(has-state? cls state-name)`](https://gamelisp.rs/std/has-state-p).
	*/
	pub fn has_state<S: ToSym>(&self, state_name: S) -> GResult<bool> {
		let sym = state_name.to_sym()?;
		Ok(self.states.contains_key(&sym))
	}

	/**
	Returns `true` if the class incorporated the given class as a mixin.

	Equivalent to [`(class-has-mixin? cls mixin)`](https://gamelisp.rs/std/class-has-mixin-p).
	*/
	pub fn has_mixin(&self, mixin: &Root<Class>) -> bool {
		self.is.contains(mixin.as_gc())
	}

	/**
	Returns `true` if the class is a mixin.

	Equivalent to [`(mixin? cls)`](https://gamelisp.rs/std/mixin-p).
	*/
	pub fn is_mixin(&self) -> bool {
		self.is_mixin
	}

	/**
	Returns all of the class' mixins, as an [`Arr`](struct.Arr.html).

	Equivalent to [`(class-mixins cls)`](https://gamelisp.rs/std/class-mixins).
	*/
	pub fn mixins(&self) -> Root<Arr> {
		//todo: make this into an iterator, and return mixins from left to right (they're currently
		//returned in an unspecified order)

		glsp::arr_from_iter(self.is.iter().map(|mixin| {
			Slot::Class(mixin.clone())
		})).unwrap()
	}
}

//vm.rs currently needs to be able to copy its callee to a Slot, so we can't implement CallableOps
//for Class or &Class. this is consistent with the current behaviour for Root<GFn>
impl CallableOps for Root<Class> {
	fn receive_call(&self, arg_count: usize) -> GResult<Val> {
		Ok(Val::Obj(glsp::call_class(self, arg_count)?))
	}

	fn arg_limits(&self) -> (usize, Option<usize>) {
		let (gfn, has_nbi) = match self.states.get(&MAIN_SYM).unwrap().init {
			Some(MethBinding::Simple(_, ref gfn, has_nbi)) => (gfn.clone(), has_nbi),
			Some(MethBinding::Stackable(_, i)) => {
				match self.meth_stack[i as usize] {
					MethStackEntry::Meth(_, ref gfn, has_nbi) => (gfn.clone(), has_nbi),
					_ => unreachable!()
				}
			}
			None => {
				//classes with no (meth init) must only receive zero init arguments
				return (0, Some(0))
			}
		};

		let (raw_min_args, raw_max_args) = gfn.arg_limits();
		let difference = if has_nbi { 2 } else { 1 };

		let min_args = raw_min_args.checked_sub(difference).unwrap();
		let max_args = raw_max_args.map(|max_args| max_args.checked_sub(difference).unwrap());
		(min_args, max_args)
	}

	fn name(&self) -> Option<Sym> {
		self.name
	}
}

impl CallableOps for Gc<Class> {
	fn receive_call(&self, arg_count: usize) -> GResult<Val> {
		self.root().receive_call(arg_count)
	}

	fn arg_limits(&self) -> (usize, Option<usize>) {
		self.root().arg_limits()
	}

	fn name(&self) -> Option<Sym> {
		self.root().name()
	}
}

//the result of trying to access a symbol in an Obj, for either a method call (.x y) or a field 
//access [x 'y]
enum Lookup {
	FieldOrConst(Slot),
	Meth(MethLookup),
	PropGetter(MethLookup),
	NotBound
}

//the result of trying to access a symbol in an Obj for a field assignment, (= [x 'y] z)
enum LookupMut<'a> {
	Field(RefMut<'a, Slot>),
	PropSetter(MethLookup),
	Error(&'static str) //"a const", "a method", "a readonly prop", or "not bound"
}

pub(crate) struct MethLookup {
	pub(crate) gfn: Gc<GFn>,
	pub(crate) requires_next_index: bool,
	pub(crate) next_index: Option<u16>
}

impl Obj {
	pub(crate) fn new<A>(class: &Root<Class>, args: &A) -> GResult<Root<Obj>> 
	where
		A: ToCallArgs + ?Sized
	{
		ensure!(!class.is_mixin, "{} is a mixin; mixins cannot be instantiated", 
		        class.name.unwrap());

		let obj = Obj {
			header: GcHeader::new(),
			class: Gc::from_root(class),
			storage: RefCell::new(None)
		};

		let root = glsp::alloc(obj);
		let prev_usage = root.memory_usage();

		*root.storage.borrow_mut() = Some(ObjStorage {
			fields: vec![Slot::Nil; class.field_count],
			states_enabled: 0,
			gc_self: root.to_gc()
		});

		with_heap(|heap| heap.memory_usage_barrier(&*root, prev_usage, root.memory_usage()));

		root.enab(MAIN_SYM, args)?;
		Ok(root)
	}

	//run any finalizers, then kill the object. if an error bubbles through, kill the object
	//without running any of the remaining finalizers.

	/**
	Kills the object.

	Equivalent to [`(obj-kill! ob)`](https://gamelisp.rs/std/obj-kill-mut).
	*/
	pub fn kill(&self) -> GResult<()> {
		if self.storage.borrow().is_some() {
			self.disab_impl(MAIN_SYM)?;
			self.kill_impl();
		}

		Ok(())
	}

	//kill the object without running finalizers
	fn kill_impl(&self) {
		let prev_usage = self.memory_usage();
		*self.storage.borrow_mut() = None;
		with_heap(|heap| {
			heap.memory_usage_barrier(self, prev_usage, self.memory_usage());
		});
	}

	/**
	Returns `true` if the object has been killed.
	
	Equivalent to [`(obj-killed? ob)`](https://gamelisp.rs/std/obj-killed-p).
	*/
	pub fn is_killed(&self) -> bool {
		self.storage.borrow().is_none()
	}

	/**
	Makes the object immutable.
	
	Equivalent to [`(freeze! ob)`](https://gamelisp.rs/std/freeze-mut).
	*/
	pub fn freeze(&self) {
		self.header.freeze()
	}

	/**
	Returns `true` if the object has been frozen.
	*/
	pub fn is_frozen(&self) -> bool {
		self.header.frozen()
	}

	/**
	Returns the object's class.
	
	Equivalent to [`(class-of ob)`](https://gamelisp.rs/std/class-of).
	*/
	pub fn class(&self) -> Root<Class> {
		self.class.root()
	}

	/**
	Returns `true` if the object is an instance of the given class or mixin.
	
	Equivalent to [`(is? ob cls)`](https://gamelisp.rs/std/is-p).
	*/
	pub fn is(&self, class: &Root<Class>) -> bool {
		Root::ptr_eq(&self.class.root(), class) || self.class.has_mixin(class)
	}

	//the common backing function for get(), call(), has(), etc. 
	#[inline(always)]
	fn lookup(&self, key: Sym) -> Lookup {
		let storage_ref = self.storage.borrow();
		let storage = match storage_ref.as_ref() {
			Some(storage) => storage,
			None => return Lookup::NotBound
		};

		let states_enabled = storage.states_enabled;
		let state_is_accessible = |state_index: u8| {
			states_enabled & (1 << state_index as u32) != 0
		};

		if let Some(binding) = self.class.bindings.get(&key) {
			match *binding {
				Binding::SimpleField(state_index, field_index) => {
					if state_is_accessible(state_index) {
						Lookup::FieldOrConst(storage.fields[field_index as usize].clone())
					} else {
						Lookup::NotBound
					}
				}
				Binding::SimpleConst(state_index, ref slot) => {
					if state_is_accessible(state_index) {
						Lookup::FieldOrConst(slot.clone())
					} else {
						Lookup::NotBound
					}
				}
				Binding::StackableField(mut stack_index) => {
					loop {
						match self.class.field_stack[stack_index as usize] {
							FieldStackEntry::Field(state_index, field_index) => {
								if state_is_accessible(state_index) {
									let i = field_index as usize;
									return Lookup::FieldOrConst(storage.fields[i].clone())
								}
							}
							FieldStackEntry::Const(state_index, ref slot) => {
								if state_is_accessible(state_index) {
									return Lookup::FieldOrConst(slot.clone())
								}
							}
							FieldStackEntry::End => return Lookup::NotBound 
						}

						stack_index += 1;
					}
				}
				Binding::Meth(ref meth_binding) => {
					match self.lookup_meth(storage, meth_binding) {
						Some(meth_lookup) => Lookup::Meth(meth_lookup),
						None => Lookup::NotBound
					}
				}
				Binding::Prop(ref get_binding, _) => {
					if let Some(ref get_binding) = *get_binding {
						match self.lookup_meth(storage, get_binding) {
							Some(get_lookup) => Lookup::PropGetter(get_lookup),
							None => Lookup::NotBound
						}
					} else {
						Lookup::NotBound
					}
				}
			}
		} else {
			Lookup::NotBound
		}
	}

	//convert a MethBinding into a MethLookup, which contains the information required to
	//actually call the method. returns None if the method isn't currently enabled in any state
	#[inline(always)]
	fn lookup_meth(&self, storage: &ObjStorage, binding: &MethBinding) -> Option<MethLookup> {
		let states_enabled = storage.states_enabled;
		let state_is_accessible = |state_index: u8| {
			states_enabled & (1 << state_index as u32) != 0
		};

		match *binding {
			MethBinding::Simple(state_index, ref gfn, requires_next_index) => {
				if state_is_accessible(state_index) {
					Some(MethLookup {
						gfn: gfn.clone(),
						requires_next_index, 
						next_index: None
					})
				} else {
					None
				}
			}
			MethBinding::Stackable(state_index, mut stack_index) => {
				if state_is_accessible(state_index) {
					loop {
						match self.class.meth_stack[stack_index as usize] {
							MethStackEntry::Meth(state_index, ref gfn, requires_next_index) => {
								if state_is_accessible(state_index) {
									let i = (stack_index + 1) as usize;
									let next_index = match self.class.meth_stack[i] {
										MethStackEntry::Meth(..) => Some((stack_index + 1) as u16),
										MethStackEntry::End => None
									};

									return Some(MethLookup {
										gfn: gfn.clone(), 
										requires_next_index, 
										next_index
									})
								} else {
									stack_index += 1;
								}
							}
							MethStackEntry::End => return None
						}
					}
				} else {
					None
				}
			}
		}
	}

	#[inline(always)]
	fn lookup_mut(&self, key: Sym) -> LookupMut {
		let mut storage_ref = self.storage.borrow_mut();
		let storage = match storage_ref.as_mut() {
			Some(storage) => storage,
			None => return LookupMut::Error("not bound")
		};

		let states_enabled = storage.states_enabled;
		let state_is_accessible = |state_index: u8| {
			states_enabled & (1 << state_index as u32) != 0
		};

		if let Some(binding) = self.class.bindings.get(&key) {
			match *binding {
				Binding::SimpleField(state_index, field_index) => {
					if state_is_accessible(state_index) {
						let rm = RefMut::map(storage_ref, |s| {
							&mut s.as_mut().unwrap().fields[field_index as usize]
						});
						LookupMut::Field(rm)
					} else {
						LookupMut::Error("not bound")
					}
				}
				Binding::StackableField(mut stack_index) => {
					loop {
						match self.class.field_stack[stack_index as usize] {
							FieldStackEntry::Field(state_index, field_index) => {
								if state_is_accessible(state_index) {
									let rm = RefMut::map(storage_ref, |s| {
										&mut s.as_mut().unwrap().fields[field_index as usize]
									});
									return LookupMut::Field(rm)
								}
							}
							FieldStackEntry::Const(state_index, _) => {
								if state_is_accessible(state_index) {
									return LookupMut::Error("a const")
								}
							}
							FieldStackEntry::End => return LookupMut::Error("not bound")
						}

						stack_index += 1;
					}
				}
				Binding::SimpleConst(state_index, _) => {
					if state_is_accessible(state_index) {
						LookupMut::Error("a const")
					} else {
						LookupMut::Error("not bound")
					}
				}
				Binding::Prop(_, ref set_binding) => {
					if let Some(ref set_binding) = *set_binding {
						match self.lookup_meth(storage, set_binding) {
							Some(set_lookup) => LookupMut::PropSetter(set_lookup),
							None => LookupMut::Error("not bound")
						}
					} else {
						LookupMut::Error("a readonly prop")
					}
				}
				Binding::Meth(_) => LookupMut::Error("a method")
			}
		} else {
			LookupMut::Error("not bound")
		}
	}

	/**
	Returns `true` if the given name is currently bound to a field, constant or property.
	
	Equivalent to [`(has? ob key)`](https://gamelisp.rs/std/has-p).
	*/
	pub fn has<S: ToSym>(&self, key: S) -> GResult<bool> {
		match self.lookup(key.to_sym()?) {
			Lookup::FieldOrConst(_) | Lookup::PropGetter(_) => Ok(true),
			Lookup::Meth(_) | Lookup::NotBound => Ok(false)
		}
	}

	/**
	Returns `true` if the given name is currently bound to a method.
	
	Equivalent to [`(has-meth? ob key)`](https://gamelisp.rs/std/has-meth-p).
	*/
	pub fn has_meth<S: ToSym>(&self, key: S) -> GResult<bool> {
		match self.lookup(key.to_sym()?) {
			Lookup::Meth(..) => Ok(true),
			Lookup::FieldOrConst(Slot::GFn(_)) => Ok(true),
			Lookup::FieldOrConst(Slot::RFn(_)) => Ok(true),
			Lookup::FieldOrConst(Slot::Class(_)) => Ok(true),
			Lookup::FieldOrConst(_) => Ok(false),
			Lookup::PropGetter(getter) => {
				match self.invoke_method(&getter, &())? {
					Slot::GFn(_) | Slot::RFn(_) | Slot::Class(_) => Ok(true),
					_ => Ok(false)
				}
			}
			Lookup::NotBound => Ok(false)
		}
	}

	/**
	Accesses the value of a field, constant or property.
	
	Equivalent to [`[ob key]`](https://gamelisp.rs/std/access).
	*/
	pub fn get<S: ToSym, V: FromVal>(&self, key: S) -> GResult<V> {
		ensure!(self.storage.borrow().is_some(),
		        "attempted to access a field on a killed obj");

		let sym = key.to_sym()?;
		match self.lookup(sym) {
			Lookup::FieldOrConst(slot) => Ok(V::from_slot(&slot)?),
			Lookup::PropGetter(getter) => {
				self.invoke_method(&getter, &())
			}
			Lookup::Meth(..) => bail!("attempted to access method '{}' as a field", sym),
			Lookup::NotBound => bail!("attempted to access nonexistent field '{}'", sym)
		}
	}

	/**
	Accesses the value of a field, constant or property, if it exists.
	
	Equivalent to [`[ob (? key)]`](https://gamelisp.rs/std/access).
	*/
	pub fn get_if_present<S: ToSym, V: FromVal>(&self, key: S) -> GResult<Option<V>> {
		if self.storage.borrow().is_some() {
			match self.lookup(key.to_sym()?) {
				Lookup::FieldOrConst(slot) => Ok(Some(V::from_slot(&slot)?)),
				Lookup::PropGetter(getter) => {
					Ok(Some(self.invoke_method(&getter, &())?))
				}
				Lookup::Meth(..) | Lookup::NotBound => Ok(None)
			}
		} else {
			Ok(None)
		}
	}

	/**
	Creates an indexing iterator for this collection.
	
	Equivalent to [`[ob iter]`](https://gamelisp.rs/std/access).
	*/
	pub fn access_giter(obj: &Root<Obj>, giter: &Root<GIter>) -> Root<GIter> {
		glsp::giter(GIterState::AccessObj(obj.to_gc(), giter.to_gc()))
	}

	//returns the information required for vm.rs to invoke a method. the first Slot is always a 
	//Slot::GFn, Slot::RFn or Slot::Class. the two bool flags are requires_self and requires_next_index.
	//the final Slot is the next_index: #n or an int.
	pub(crate) fn get_method(
		&self, 
		method_name: Sym
	) -> GResult<Option<(Slot, bool, bool, Slot)>> 
	{
		Ok(match self.lookup(method_name) {
			Lookup::FieldOrConst(slot) => {
				match slot {
					Slot::GFn(_) | Slot::RFn(_) | Slot::Class(_) => {
						Some((slot, false, false, Slot::Nil))
					}
					_ => None
				}
			}
			Lookup::PropGetter(getter) => {
				let slot = self.invoke_method(&getter, &())?;
				match slot {
					Slot::GFn(_) | Slot::RFn(_) | Slot::Class(_) => {
						Some((slot, false, false, Slot::Nil))
					}
					_ => None
				}
			}
			Lookup::Meth(MethLookup { gfn, requires_next_index, next_index }) => {
				let ni_slot = match next_index {
					Some(ni) => Slot::Int(ni as i32),
					None => Slot::Nil
				};

				Some((Slot::GFn(gfn), true, requires_next_index, ni_slot))
			}
			Lookup::NotBound => None
		})
	}

	//used by OpCallBaseRaw
	pub(crate) fn get_base_raw_method(&self, mut index: usize) -> Option<MethLookup> {
		let states_enabled = self.storage.borrow().as_ref().unwrap().states_enabled;

		loop {
			match self.class.meth_stack[index] {
				MethStackEntry::End => {
					return None
				}
				MethStackEntry::Meth(state_id, ref gfn, requires_next_index) => { 
					if states_enabled & (1 << state_id as u32) != 0 {
						let next_index = match self.class.meth_stack[index + 1] {
							MethStackEntry::End => None,
							_ => Some((index + 1) as u16)
						};

						return Some(MethLookup {
							gfn: gfn.clone(),
							requires_next_index, 
							next_index
						})
					} else {
						index += 1
					}
				}
			}
		}
	}

	//used by the undocumented call-base-raw function in glsp-stdlib
	#[doc(hidden)]
	pub fn raw_call<A>(&self, index: usize, args: &A) -> GResult<Val> 
	where
		A: ToCallArgs + ?Sized
	{
		match self.get_base_raw_method(index) {
			Some(meth_lookup) => self.invoke_method(&meth_lookup, args),
			None => Ok(Val::Nil)
		}
	}

	/**
	Invokes a method.
	
	Equivalent to [`(call-meth ob key ..args)`](https://gamelisp.rs/std/call-meth).
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
	
	Equivalent to [`(call-meth ob (? key) ..args)`](https://gamelisp.rs/std/call-meth).
	*/
	pub fn call_if_present<S, A, R>(&self, key: S, args: &A) -> GResult<Option<R>> 
	where
		S: ToSym,
		A: ToCallArgs + ?Sized, 
		R: FromVal
	{
		let sym = key.to_sym()?;

		if self.storage.borrow().is_some() {
			let slot = match self.lookup(sym) {
				Lookup::Meth(meth_lookup) => {
					return Ok(Some(self.invoke_method(&meth_lookup, args)?))
				}
				Lookup::FieldOrConst(slot) => slot,
				Lookup::PropGetter(getter) => self.invoke_method(&getter, &())?,
				Lookup::NotBound => return Ok(None)
			};

			match slot {
				Slot::GFn(gfn) => Ok(Some(glsp::call(&gfn, args)?)),
				Slot::RFn(rfn) => Ok(Some(glsp::call(&rfn, args)?)),
				Slot::Class(class_to_call) => Ok(Some(glsp::call(&class_to_call, args)?)),
				_ => Ok(None)
			}
		} else {
			Ok(None)
		}
	}

	//invoke a method represented by a MethLookup. the `self` and `next_index` args are implicit.
	fn invoke_method<A, R>(&self, meth_lookup: &MethLookup, args: &A) -> GResult<R> 
	where
		A: ToCallArgs + ?Sized, 
		R: FromVal
	{
		with_vm(|vm| {
			let mut stacks = vm.stacks.borrow_mut();
			let starting_len = stacks.regs.len();

			stacks.regs.push(Slot::Obj(self.storage.borrow().as_ref().unwrap().gc_self.clone()));

			if meth_lookup.requires_next_index {
				let next_index_slot = match meth_lookup.next_index {
					Some(next_index) => Slot::Int(next_index as i32),
					None => Slot::Nil
				};
				stacks.regs.push(next_index_slot);
			}

			args.to_call_args(&mut stacks.regs)?;

			let arg_count = stacks.regs.len() - starting_len;
			drop(stacks);

			let val = meth_lookup.gfn.root().receive_call(arg_count)?;
			R::from_val(&val)
		})
	}

	fn set_impl<S: ToSym, V: ToVal>(&self, key: S, value: V) -> GResult<Option<&'static str>> {
		ensure!(!self.header.frozen(), "attempted to mutate a frozen obj");
		ensure!(self.storage.borrow().is_some(), "attempted to mutate a field on a killed obj");

		let sym = key.to_sym()?;
		match self.lookup_mut(sym) {
			LookupMut::Field(mut field) => {
				let slot = value.to_slot()?;
				with_heap(|heap| heap.write_barrier_slot(self, &slot));
				*field = slot;
				Ok(None)
			}
			LookupMut::PropSetter(setter) => {
				let _: Slot = self.invoke_method(&setter, &[value])?;
				Ok(None)
			}
			LookupMut::Error(msg) => Ok(Some(msg))
		}
	}

	/**
	Mutates the field or property bound to the given name.
	
	Equivalent to [`(= [ob key] value)`](https://gamelisp.rs/std/set-access).
	*/
	pub fn set<S: ToSym, V: ToVal>(&self, key: S, value: V) -> GResult<()> {
		let key_sym = key.to_sym()?;

		match self.set_impl(key_sym, value)? {
			Some(msg) => bail!("attempted to set field '{}', which is {}", key_sym, msg),
			None => Ok(())
		}
	}

	/**
	Mutates the field or property bound to the given name, if any. Returns `true` if 
	the field or property exists.
	
	Equivalent to [`(= [ob (? key)] value)`](https://gamelisp.rs/std/set-access).
	*/
	pub fn set_if_present<S: ToSym, V: ToVal>(&self, key: S, value: V) -> GResult<bool> {
		Ok(self.set_impl(key, value)?.is_none())
	}

	/**
	Returns `true` if the object's class defines a state with the given name, even if it's not
	currently enabled.
	
	Equivalent to [`(has-state? ob state-name)`](https://gamelisp.rs/std/has-state-p).
	*/
	pub fn has_state<S: ToSym>(&self, state_name: S) -> GResult<bool> {
		Ok(self.class.states.contains_key(&state_name.to_sym()?))
	}

	/**
	Returns `true` if a state is currently enabled.
	
	Equivalent to [`(enab? ob state-name)`](https://gamelisp.rs/std/enab-p).
	*/
	pub fn is_enab<S: ToSym>(&self, state_name: S) -> GResult<bool> {
		let sym = state_name.to_sym()?;

		let storage_ref = self.storage.borrow();
		let storage = match storage_ref.as_ref() {
			Some(storage) => storage,
			None => bail!("attempted to query the state '{}' on a killed obj", sym)
		};

		match self.class.states.get(&sym) {
			Some(state_ref) => Ok(storage.states_enabled & (1 << state_ref.index as u32) != 0),
			None => bail!("attempted to query a nonexistent state '{}'", sym)
		}
	}

	/**
	Enables a state.
	
	Equivalent to [`(enab! ob state-name)`](https://gamelisp.rs/std/enab-mut).
	*/
	pub fn enab<S, A>(&self, state_name: S, args: &A) -> GResult<()>
	where
		S: ToSym,
		A: ToCallArgs + ?Sized
	{
		let sym = state_name.to_sym()?;

		ensure!(self.class.states.contains_key(&sym),
		        "attempted to enable the nonexistent state '{}'", sym);
		ensure!(!self.header.frozen(),
		        "attempted to enable the state '{}' on a frozen obj", sym);
		ensure!(self.storage.borrow().is_some(),
		        "attempted to enable the state '{}' on a killed obj", sym);
		
		self.recursively_enable_state(sym, args)
	}

	//enables/disables this state's parent, children and fsm-siblings as appropriate, and also
	//enables the state itself
	fn recursively_enable_state<A: ToCallArgs + ?Sized>(
		&self,
		state_name: Sym,
		args: &A
	) -> GResult<()> {

		//the sequence is:
		// - if this state's immediate parent isn't enabled, enable it (recursively)
		//		- this may cause the parent to enable an fsm-sibling or enable this state
		//      - at this stage, if we find that we've already been enabled, it will already
		//        recursed, which means that our job is done and we can return early
		// - if this state has an enabled fsm-sibling, disable it (recursively)
		//		- this will disable the fsm-sibling's children
		// - enable this state (non-recursively)
		// - for each of this state's children, if it's a `state*`, enable it (recursively)
		//
		//in the event that any of the state initializers fail, it's particularly likely that the 
		//obj will be left in an incoherent state, because initialization is complex and finicky.
		//we take a page from rust's book and "poison" the obj by killing it if an exception
		//bubbles through it during initialization. 

		let guard = Guard::new(|| self.kill_impl());

		let state_ref = self.class.states.get(&state_name).unwrap();
		let mut states_enabled = self.storage.borrow().as_ref().unwrap().states_enabled;

		if let Some(parent_name) = state_ref.parent {
			let parent_ref = self.class.states.get(&parent_name).unwrap();
			if states_enabled & (1 << parent_ref.index as u32) == 0 {
				self.recursively_enable_state(parent_name, args)?;

				states_enabled = self.storage.borrow().as_ref().unwrap().states_enabled;

				if states_enabled & (1 << state_ref.index as u32) != 0 {
					forget(guard);
					return Ok(())
				}
			}
		}

		for sibling_name in &state_ref.fsm_siblings {
			let sibling_ref = self.class.states.get(&sibling_name).unwrap();
			if states_enabled & (1 << sibling_ref.index as u32) != 0 {
				self.disab(*sibling_name)?;
				//states_enabled = self.storage.borrow().as_ref().unwrap().states_enabled;
				break
			}
		}

		self.enable_state(state_name, args)?;
		states_enabled = self.storage.borrow().as_ref().unwrap().states_enabled;

		for child_name in &state_ref.children {
			let child_ref = self.class.states.get(child_name).unwrap();
			
			if child_ref.enabled_by_default {
				//we can't assume that the child state is disabled here, because this state's
				//`init` method could have explicitly called `enab!`
				if states_enabled & (1 << child_ref.index as u32) == 0 {
					let empty_args: &[Slot] = &[];
					self.recursively_enable_state(*child_name, empty_args)?;
				}
			}
		}

		forget(guard);
		Ok(())
	}

	//enables a state and calls its `init` method, but does not propagate to the state's children, 
	//parent or fsm-siblings
	fn enable_state<A: ToCallArgs + ?Sized>(
		&self, 
		state_name: Sym, 
		args: &A
	) -> GResult<()> {

		let mut storage_ref = self.storage.borrow_mut();
		let storage = storage_ref.as_mut().unwrap();

		let state_ref = self.class.states.get(&state_name).unwrap();
		let state_bit = 1 << state_ref.index as u32;

		ensure!(storage.states_enabled & state_bit == 0,
		        "attempted to enable the state {}, which is already enabled", state_name);

		storage.states_enabled |= state_bit;
		let states_enabled = storage.states_enabled;

		//check that we're not violating `requires` or `excludes` by enabling this state
		ensure!((states_enabled & state_ref.requires) == state_ref.requires,
		        "attempted to enable {}, but it wraps a method in a state which is not enabled",
		        state_ref.name);
		ensure!((states_enabled & state_ref.excludes) == 0, "attempted to enable {}, but \
		        this caused a method name collision with another state", state_ref.name);

		//call the initializer method
		if let Some(ref init_binding) = state_ref.init {
			let init_lookup = self.lookup_meth(storage, init_binding).unwrap();
			drop(storage_ref);
			let _: Slot = self.invoke_method(&init_lookup, args)?;
		} else {
			//if there's no initializer, check that we received zero args
			ensure!(args.arg_count() == 0, "too many arguments to state {}'s initializer: \
			        expected 0, received {}", state_name, args.arg_count());
		}

		Ok(())
	}

	/**
	Disables a state.

	Equivalent to [`(disab! ob state-name)`](https://gamelisp.rs/std/disab-mut).
	*/
	pub fn disab<S: ToSym>(&self, state_name: S) -> GResult<()> {
		let state_name_sym = state_name.to_sym()?;
		ensure!(state_name_sym != MAIN_SYM, "the `Main` state cannot be disabled");

		self.disab_impl(state_name_sym)
	}

	fn disab_impl<S: ToSym>(&self, state_name: S) -> GResult<()> {
		let state_name_sym = state_name.to_sym()?;

		ensure!(!self.header.frozen(),
		        "attempted to disable the state '{}' on a frozen obj", state_name_sym);
		ensure!(self.storage.borrow().is_some(),
		        "attempted to disable the state '{}' on a killed obj", state_name_sym);

		//just like initializers, we kill the object if a finalizer fails
		let guard = Guard::new(|| self.kill_impl());

		//disable all of the immediate child states.
		let states_enabled = self.storage.borrow().as_ref().unwrap().states_enabled;
		let state_ref = match self.class.states.get(&state_name_sym) {
			Some(state_ref) => state_ref,
			None => bail!("attempted to disable the nonexistent state '{}'", state_name_sym)
		};

		for child_name in state_ref.children.iter().rev() {
			let child_ref = self.class.states.get(&child_name).unwrap();
			if states_enabled & (1 << child_ref.index as u32) != 0 {
				self.disab(*child_name)?;
			}
		}

		//call the finalizer methods
		for fini_binding in state_ref.finis.iter().rev() {
			let storage = self.storage.borrow();
			let fini_lookup = self.lookup_meth(
				&storage.as_ref().unwrap(), 
				&fini_binding
			).unwrap();
			drop(storage);
			let _: Slot = self.invoke_method(&fini_lookup, &())?;
		}

		//disable the state's fields etc., unless it's Main
		if state_name_sym != MAIN_SYM {
			self.disable_state(state_name_sym)?;
		}

		forget(guard);
		Ok(())
	}

	fn disable_state(&self, state_name: Sym) -> GResult<()> {
		let mut storage_ref = self.storage.borrow_mut();
		let storage = storage_ref.as_mut().unwrap();

		let state_ref = self.class.states.get(&state_name).unwrap();
		let state_bit = 1 << state_ref.index as u32;

		ensure!(storage.states_enabled & state_bit != 0,
		        "attempted to disable the state {}, which is already disabled", state_name);

		//check that we're not violating `required_by` when we disable this state
		ensure!((storage.states_enabled & state_ref.required_by) == 0, "attempted to disable {}, \
		        but an active state is currently wrapping one of its methods", state_ref.name);

		//we need to replace any fields which belong to this state with #n, so that they
		//don't live too long for gc purposes, or reappear when the state is re-enabled
		for binding in self.class.bindings.values() {
			if let Binding::SimpleField(field_state_index, field_index) = *binding {
				if field_state_index == state_ref.index as u8 {
					storage.fields[field_index as usize] = Slot::Nil;
				}
			}
		}

		for entry in &self.class.field_stack {
			if let FieldStackEntry::Field(field_state_index, field_index) = *entry {
				if field_state_index == state_ref.index as u8 {
					storage.fields[field_index as usize] = Slot::Nil;
				}
			}
		}

		//after that, we simply clear the state's bit in the obj
		storage.states_enabled &= !state_bit;
		Ok(())
	}

	/**
	Equivalent to [`(eq? self other)`](https://gamelisp.rs/std/eq-p).

	Note that, because this may invoke an `op-eq?` method, it can potentially fail.

	The same is true for `PartialEq` comparisons between objects using Rust's `==` operator.
	In that case, if an error occurs, the operator will panic.
	*/
	pub fn try_eq(&self, other: &Root<Obj>) -> GResult<bool> {
		if !Root::ptr_eq(&self.class(), &other.class()) {
			return Ok(false)
		}

		let val: Option<Val> = self.call_if_present(OP_EQP_SYM, &[other])?;
		match val {
			Some(val) => Ok(val.is_truthy()),
			None => Ok(false)
		}
	}
}

impl PartialEq<Root<Obj>> for Obj {
	fn eq(&self, other: &Root<Obj>) -> bool {
		self.try_eq(other).unwrap()
	}
}


//-------------------------------------------------------------------------------------------------
// Allocate implementations
//-------------------------------------------------------------------------------------------------

impl Allocate for Obj {
	fn header(&self) -> &GcHeader {
		&self.header
	}

	fn visit_gcs<V: Visitor>(&self, visitor: &mut V) {
		visitor.visit_gc(&self.class);
		
		let storage_ref = self.storage.borrow();
		if let Some(ref storage) = *storage_ref {
			for field in &storage.fields {
				visitor.visit_slot(field);
			}

			visitor.visit_gc(&storage.gc_self);
		}
	}

	fn clear_gcs(&self) {
		*self.storage.borrow_mut() = None;
	}

	fn owned_memory_usage(&self) -> usize {
		let storage_ref = self.storage.borrow();
		if let Some(ref storage) = *storage_ref {
			storage.fields.capacity() * size_of::<Slot>()
		} else {
			0
		}
	}
}

impl Allocate for Class {
	fn header(&self) -> &GcHeader {
		&self.header
	}

	fn visit_gcs<V: Visitor>(&self, visitor: &mut V) {
		for binding in self.bindings.values() {
			match *binding {
				Binding::SimpleField(_, _) => (),
				Binding::SimpleConst(_, ref val) => visitor.visit_slot(val),
				Binding::StackableField(_) => (),
				Binding::Meth(MethBinding::Simple(_, ref gfn, _)) => visitor.visit_gc(gfn),
				Binding::Meth(MethBinding::Stackable(_, _)) => (),
				Binding::Prop(ref getter, ref setter) => {
					if let Some(MethBinding::Simple(_, ref gfn, _)) = *getter {
						visitor.visit_gc(gfn);
					}
					if let Some(MethBinding::Simple(_, ref gfn, _)) = *setter {
						visitor.visit_gc(gfn);
					}
				}
			}
		}

		for entry in &self.field_stack {
			match *entry {
				FieldStackEntry::Field(_, _) => (),
				FieldStackEntry::Const(_, ref val) => visitor.visit_slot(val),
				FieldStackEntry::End => ()
			}
		}

		for entry in &self.meth_stack {
			match *entry {
				MethStackEntry::Meth(_, ref gfn, _) => visitor.visit_gc(gfn),
				MethStackEntry::End => ()
			}
		}

		for class in &self.is {
			visitor.visit_gc(class);
		}

		for state in self.states.values() {
			for meth_binding in state.init.iter().chain(state.finis.iter()) {
				match *meth_binding {
					MethBinding::Simple(_, ref gfn, _) => visitor.visit_gc(gfn),
					MethBinding::Stackable(_, _) => ()
				}
			}
		}

		if let Some(raw_class) = self.raw_class.as_ref() {
			for class in &raw_class.mixins {
				visitor.visit_gc(class);
			}

			for raw_init in raw_class.inits.iter().chain(raw_class.finis.iter()) {
				visitor.visit_gc(&raw_init.gfn);
			}

			for binding in &raw_class.bindings {
				match binding.bindee {
					RawBindee::Field => (),
					RawBindee::PreConst(ref gfn) => {
						if let Some(ref gfn) = *gfn {
							visitor.visit_gc(gfn)
						}
					}
					RawBindee::Const(ref slot) => visitor.visit_slot(slot),

					RawBindee::Meth(ref gfn) => visitor.visit_gc(gfn),
					RawBindee::Wrap(_, ref gfn) => visitor.visit_gc(gfn),
					RawBindee::WildcardWrap(ref gfn) => visitor.visit_gc(gfn), 

					RawBindee::Prop(_, _, ref get, ref set) |
					RawBindee::WrapProp(_, _, _, ref get, ref set) |
					RawBindee::WildcardWrapProp(_, _, ref get, ref set) => {
						if let Some(ref get) = *get {
							visitor.visit_gc(get);
						}
						if let Some(ref set) = *set {
							visitor.visit_gc(set);
						}
					}
				}
			}
		}
	}

	fn clear_gcs(&self) {
		//deliberate no-op
	}

	//i think this is currently a fairly rough estimate
	fn owned_memory_usage(&self) -> usize {
		let basic = self.bindings.capacity() * size_of::<(Sym, Binding)>()
		+ self.field_stack.capacity() * size_of::<FieldStackEntry>()
		+ self.meth_stack.capacity() * size_of::<MethStackEntry>()
		+ self.is.capacity() * size_of::<Gc<Class>>()
		+ self.states.capacity() * size_of::<(Sym, State)>();

		let mut states = 0usize;
		for state in self.states.values() {
			states += state.fsm_siblings.capacity() * size_of::<Sym>();
			states += state.children.capacity() * size_of::<Sym>();
			states += state.finis.capacity() * size_of::<MethBinding>();
		}

		let raw_class = if let Some(raw_class) = self.raw_class.as_ref() {
			let mut raw = size_of::<RawClass>() +
			raw_class.mixins.capacity() * size_of::<Vec<Gc<Class>>>() +
			raw_class.states.capacity() * size_of::<State>() +
			raw_class.bindings.capacity() * size_of::<RawBinding>() +
			raw_class.inits.capacity() * size_of::<RawInit>() +
			raw_class.finis.capacity() * size_of::<RawInit>();

			for state in &raw_class.states {
				raw += state.fsm_siblings.capacity() * size_of::<Sym>();
				raw += state.children.capacity() * size_of::<Sym>();
				raw += state.finis.capacity() * size_of::<MethBinding>();
			}

			raw
		} else {
			0
		};

		basic + states + raw_class
	}
}


//-------------------------------------------------------------------------------------------------
// RawClass and (%make-class)
//-------------------------------------------------------------------------------------------------

struct RawClass {
	name: Option<Sym>,
	is_mixin: bool,
	mixins: Vec<Gc<Class>>,
	states: Vec<State>,
	bindings: Vec<RawBinding>,
	inits: Vec<RawInit>,
	finis: Vec<RawInit>,
}

//a RawBinding represents the partial binding introduced by a single clause: (field ...), 
//(meth ...), etc. some of the unqualified Bindings in the final class are actually the flattened 
//combination of several RawBindings.
#[derive(Clone)]
struct RawBinding {
	unqualified: Sym, //e.g. on-step
	qualified: Sym,   //e.g. Main:on-step
	state_name: Sym,  //e.g. Main
	state_i: u8,      //e.g. 0

	//at the root of a mixin, the qualified name will be MixinName:something, but the
	//state_name will still be Main. this is the only case where the qualified prefix and
	//the state_name differ.

	bindee: RawBindee
}

#[derive(Clone)]
enum RawBindee {
	Field,
	PreConst(Option<Gc<GFn>>),
	Const(Slot),

	Meth(Gc<GFn>), //meth_gfn
	Wrap(Sym, Gc<GFn>), // qualified_dst, meth_gfn
	WildcardWrap(Gc<GFn>), // meth_gfn
	
	//all start with qualified_get, qualified_set. then...
	Prop(Sym, Sym, Option<Gc<GFn>>, Option<Gc<GFn>>), //get_gfn, set_gfn
	WrapProp(Sym, Sym, Sym, Option<Gc<GFn>>, Option<Gc<GFn>>), // qualified_dst, get_gfn, set_gfn
	WildcardWrapProp(Sym, Sym, Option<Gc<GFn>>, Option<Gc<GFn>>), // get_gfn, set_gfn
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Category {
	FieldLike,
	MethLike,
	PropLike
}

impl RawBindee {
	fn category(&self) -> Category {
		use RawBindee::*;
		match self {
			Field | PreConst(_) | Const(_) => Category::FieldLike,
			Meth(_) | Wrap(_, _) | WildcardWrap(_) => Category::MethLike,
			Prop(..) | WrapProp(..) | WildcardWrapProp(..) => Category::PropLike
		}
	}
}

#[derive(Clone)]
struct RawInit {
	state_name: Sym, //Main for init, init-mixin, fini, or fini-mixin
	gfn: Gc<GFn>,
	requires_next_index: bool
}

impl RawClass {

	//a raw-class is a table with the following fields:
	//	- name: a sym; may be absent
	//  - mixin?: a bool
	//	- mixins: an arr of classes, potentially empty
	//	- states: an ordered arr of raw-state tables. "Main" is always the leftmost state
	//	- bindings: an arr of raw-binding arrs, in their textual order. each arr is one of..
	//		(unqualified-name qualified-name state-name 'field)
	//		(... 'const (? initializer-gfn))
	//		(... 'meth gfn)
	//		(... 'wrap qualified-dst gfn)
	//		(... 'wildcard-wrap gfn)
	//		(... 'prop qualified-get qualified-set get-gfn-or-nil set-gfn-or-nil)
	//		(... 'wrap-prop qualified-get qualified-set qualified-dst get-gfn-or-nil set-gfn-or-nil)
	//		(... 'wildcard-wrap-prop qualified-get qualified-set get-gfn-or-nil set-gfn-or-nil)
	//  - inits: an arr of (state-name gfn requires-next-index?) arrs
	//  - finis: an arr of (state-name gfn) arrs

	fn from_tab(tab: &Tab) -> GResult<RawClass> {
		let name = tab.get_if_present::<_, Sym>(NAME_SYM)?;
		let is_mixin = tab.get::<_, bool>(MIXINP_SYM)?;
		let mixins = tab.get::<_, Vec<Gc<Class>>>(MIXIN_SYM)?;

		let mut states = Vec::<State>::new();
		let states_arr = tab.get::<_, Gc<Arr>>(STATES_SYM)?;
		for maybe_state_tab in states_arr.iter_to::<Gc<Tab>>() {
			let state_tab = maybe_state_tab?;

			ensure!(states.len() <= 32, "{} states in a class, but the limit is 31",
		            states.len() - 1);

			let state_i: u8 = states.len() as u8;
			states.push(State::from_tab(&state_tab, state_i)?);
		}

		ensure!(!states.is_empty() && states[0].name == MAIN_SYM);
		ensure!(states[0].enabled_by_default);
		ensure!(states[0].fsm_siblings.is_empty());
		ensure!(states[0].parent.is_none());

		let state_ids = HashMap::<Sym, u8>::from_iter(states.iter()
			.enumerate()
			.map(|(i, state)| (state.name, i as u8)));

		let mut bindings = Vec::<RawBinding>::new();
		let bindings_arr: Gc<Arr> = tab.get(BINDINGS_SYM)?;
		for maybe_arr in bindings_arr.iter_to::<Gc<Arr>>() {
			let arr = maybe_arr?;

			ensure!(arr.len() >= 4);
			let unqualified: Sym = arr.get(0)?;
			let qualified: Sym = arr.get(1)?;
			let state_name: Sym = arr.get(2)?;
			let tag: Sym = arr.get(3)?;

			let bindee = match tag {
				FIELD_SYM => RawBindee::Field,
				CONST_SYM => RawBindee::PreConst(if arr.len() >= 5 { 
					Some(arr.get(4)?)
				} else {
					None
				}),
				METH_SYM => RawBindee::Meth(arr.get(4)?),
				WRAP_SYM => RawBindee::Wrap(arr.get(4)?, arr.get(5)?),
				WILDCARD_WRAP_SYM => RawBindee::WildcardWrap(arr.get(4)?),
				PROP_SYM | WRAP_PROP_SYM | WILDCARD_WRAP_PROP_SYM => {
					let qualified_get: Sym = arr.get(4)?;
					let qualified_set: Sym = arr.get(5)?;

					let getter = match arr.get(-2)? {
						Slot::Nil => None,
						Slot::GFn(gfn) => Some(gfn),
						slot => bail!("expected nil or a fn, received {}", slot.a_type_name())
					};

					let setter = match arr.get(-1)? {
						Slot::Nil => None,
						Slot::GFn(gfn) => Some(gfn),
						slot => bail!("expected nil or a fn, received {}", slot.a_type_name())
					};

					match tag {
						PROP_SYM => RawBindee::Prop(qualified_get, qualified_set, getter, setter),
						WRAP_PROP_SYM => {
							RawBindee::WrapProp(qualified_get, qualified_set, arr.get(6)?, 
							                    getter, setter)
						}
						WILDCARD_WRAP_PROP_SYM => {
							RawBindee::WildcardWrapProp(qualified_get, qualified_set, 
							                            getter, setter)
						}
						_ => unreachable!()
					}
				}
				tag => bail!("unrecognized bindee tag {}", tag)
			};

			bindings.push(RawBinding {
				unqualified,
				qualified,
				state_name,
				state_i: state_ids[&state_name],

				bindee
			});
		}

		let mut inits = Vec::<RawInit>::new();
		let inits_arr: Gc<Arr> = tab.get(INITS_SYM)?;
		for i in 0 .. inits_arr.len() {
			let (state_name, gfn, requires_next_index): (Sym, Gc<GFn>, bool) = inits_arr.get(i)?;
			inits.push(RawInit { state_name, gfn, requires_next_index });
		}

		let mut finis = Vec::<RawInit>::new();
		let finis_arr: Gc<Arr> = tab.get(FINIS_SYM)?;
		for i in 0 .. finis_arr.len() {
			let (state_name, gfn): (Sym, Gc<GFn>) = finis_arr.get(i)?;
			finis.push(RawInit { state_name, gfn, requires_next_index: false });
		}

		Ok(RawClass {
			name,
			is_mixin,
			mixins,
			states,
			bindings,
			inits,
			finis
		})
	}

	//apply all mixins
	fn mix(&mut self) -> GResult<()> {
		if !self.is_mixin && self.mixins.len() > 0 {

			//check that there are no duplicate names between all of the mixins and all of
			//the class' own states. also check that each mixin is actually a mixin!
			let mut names = HashSet::<Sym>::new();
			for state in &self.states {
				let state_name = state.name;
				ensure!(names.insert(state_name), "duplicate state name {}", state_name);
			}

			for mixin in &self.mixins {
				if !mixin.is_mixin {
					match mixin.name {
						Some(name) => bail!("{} is not a mixin", name),
						None => bail!("attempted to use a non-mixin class as a mixin")
					}
				}

				let mixin_name = mixin.name.unwrap();
				ensure!(names.insert(mixin_name), "duplicate state or mixin name {}", mixin_name);

				//each mixin's states may not collide with other states or mixins, with the sole
				//exception being when a mixin contains a single state and no other bindings, in
				//which case it can be named after the mixin
				let raw_mixin = mixin.raw_class.as_ref().unwrap();
				for state in &raw_mixin.states[1..] {
					let state_name = state.name;

					if state_name == mixin_name {
						let msg = "a mixin-state must only contain a single (state)/(state*) form";

						ensure!(raw_mixin.states.iter()
						                 .filter(|s| s.parent == Some(MAIN_SYM))
						                 .count() == 1, msg);

						for binding in &raw_mixin.bindings {
							ensure!(binding.state_name != MAIN_SYM, msg)
						}

						for raw_init in raw_mixin.inits.iter().chain(raw_mixin.finis.iter()) {
							ensure!(raw_init.state_name != MAIN_SYM, msg)
						}
					} else {
						ensure!(names.insert(state_name), "duplicate state name {}", state_name);
					}
				}
			}

			//apply all mixins
			for mixin in self.mixins.clone().into_iter().rev() {
				self.apply_mixin(mixin)?;
			}
		}

		Ok(())
	}

	fn apply_mixin(&mut self, mixin: Gc<Class>) -> GResult<()> {
		let raw_mixin = mixin.raw_class.as_ref().unwrap();

		//copy over all of the inits and finis
		self.inits.splice(0..0, raw_mixin.inits.iter().cloned());
		self.finis.splice(0..0, raw_mixin.finis.iter().cloned());

		//copy over all of the bindings. to preserve field-shadowing order, we need to end up
		//with bindings in this order: new Main, old Main, new states, old states. luckily,
		//because of the state-traversal order, all of the Main bindings will already be collected
		//at the start of each bindings list.
		let old_mains = self.bindings.iter().filter(|rb| rb.state_name == MAIN_SYM).count();
		let new_mains = raw_mixin.bindings.iter().filter(|rb| rb.state_name == MAIN_SYM).count();
		assert!(self.bindings[old_mains..].iter().all(|rb| rb.state_name != MAIN_SYM));
		assert!(raw_mixin.bindings[new_mains..].iter().all(|rb| rb.state_name != MAIN_SYM));

		self.bindings.splice(0..0, raw_mixin.bindings[..new_mains].iter().cloned());
		self.bindings.splice(new_mains + old_mains .. new_mains + old_mains, 
		                     raw_mixin.bindings[new_mains..].iter().cloned());

		//for each State in the mixin (other than Main), we've already checked that its name 
		//doesn't collide with any existing states, so we can simply copy it over and reparent it.
		//this requires us to adjust various fields throughout the RawClass...
		self.states.splice(1..1, raw_mixin.states[1..].iter().cloned());

		ensure!(self.states.len() <= 32, "a single class may not contain more than 31 states");

		//fix up State::index
		for i in 1 .. self.states.len() {
			self.states[i].index = i as u8;
		}

		//fix up RawBinding::state_i for any bindings which were present before the mixin
		let i_offset = (raw_mixin.states.len() - 1) as u8;
		for rb in old_mains + raw_mixin.bindings.len() .. self.bindings.len() {
			assert!(self.bindings[rb].state_i != 0);
			self.bindings[rb].state_i += i_offset;
		}
	
		//fix up the Main state's `children` field
		self.states[0].children.splice(0..0, raw_mixin.states.iter()
			.skip(1)
			.filter(|state| state.parent == Some(MAIN_SYM))
			.map(|state| state.name));

		Ok(())
	}
}

//we don't want to cram the entire RawClass->Class conversion into a single method body,
//so we use this struct to store some persistent state
struct ClassBuilder {
	raw_class: Box<RawClass>,
	is: FnvHashSet<Gc<Class>>,
	states: FnvHashMap<Sym, State>,
	bindings: FnvHashMap<Sym, Binding>,
	field_count: u16,
	field_stack: Vec<FieldStackEntry>,
	meth_stack: Vec<MethStackEntry>
}

impl ClassBuilder {
	fn new(raw_class: RawClass) -> GResult<ClassBuilder> {
		let is = FnvHashSet::from_iter(raw_class.mixins.iter().cloned());

		let mut states = FnvHashMap::with_capacity_and_hasher(
			raw_class.states.len(),
			Default::default()
		);
		for state in &raw_class.states {
			ensure!(states.insert(state.name, state.clone()).is_none(), 
			        "duplicate state {}", state.name);
		}

		Ok(ClassBuilder {
			raw_class: Box::new(raw_class),
			is,
			states,
			bindings: FnvHashMap::default(),
			field_count: 0,
			field_stack: Vec::new(),
			meth_stack: Vec::new()
		})
	}

	fn build(mut self) -> GResult<Class> {
		//convert any PreConst RawBindings into Consts by evaluating them in the correct order
		self.evaluate_consts()?;

		//the init MethBinding and fini MethBinding for each state, including Main, can be 
		//constructed easily enough from the `inits` and `finis` vecs in RawClass. just need to 
		//check that states other than Main don't contain any duplicate inits/finis.
		let mut main_inits = Vec::<RawInit>::new();
		let mut main_finis = Vec::<RawInit>::new();

		for init in &self.raw_class.inits {
			if init.state_name == MAIN_SYM {
				main_inits.push(init.clone());
			} else {
				let state = self.states.get_mut(&init.state_name).unwrap();
				ensure!(state.init.is_none(), "multiple (init-state) forms in {}", state.name);
				state.init = Some(MethBinding::Simple(
					state.index, 
					init.gfn.clone(),
					false
				));
			}
		}

		if main_inits.len() == 1 {
			let state = self.states.get_mut(&MAIN_SYM).unwrap();
			state.init = Some(MethBinding::Simple(
				state.index,
				main_inits[0].gfn.clone(),
				main_inits[0].requires_next_index
			));
		} else if main_inits.len() > 1 {
			let state = self.states.get_mut(&MAIN_SYM).unwrap();
			state.init = Some(MethBinding::Stackable(state.index, self.meth_stack.len() as u16));
			for init in &main_inits {
				self.meth_stack.push(MethStackEntry::Meth(
					state.index,
					init.gfn.clone(),
					init.requires_next_index
				));
			}
			self.meth_stack.push(MethStackEntry::End);
		}

		for fini in &self.raw_class.finis {
			if fini.state_name == MAIN_SYM {
				main_finis.push(fini.clone());
			} else {
				let state = self.states.get_mut(&fini.state_name).unwrap();
				ensure!(state.finis.is_empty(), "multiple (fini-state) forms in {}", state.name);
				state.finis.push(MethBinding::Simple(
					state.index, 
					fini.gfn.clone(),
					false
				));
			}
		}

		for fini in &main_finis {
			let state = self.states.get_mut(&MAIN_SYM).unwrap();
			state.finis.push(MethBinding::Simple(
				state.index,
				fini.gfn.clone(),
				false
			));
		}

		/*
		the rest of this method is dedicated to taking the list of flat RawBindings, and
		converting it into the map of Bindings, the field stack, and the method stack.

		the technique is that we group the RawBindings vec by unqualified name, check that
		all of the raw-bindings for that unqualified name belong to the same category (field-like,
		meth-like or prop-like), and then branch for that category.

		for the time being, we've written the RawBindings processing code to be clear and simple,
		rather than fast. might try to improve performance once it's more stable (todo).
		*/

		//this is a stable sort
		let mut grouped = self.raw_class.bindings.clone();
		grouped.sort_by(|a, b| a.unqualified.0.cmp(&b.unqualified.0));

		let mut group = VecDeque::<RawBinding>::with_capacity(16);
		while grouped.len() > 0 {
			//collect all RawBindings which share the same unqualified name
			group.clear();
			group.push_front(grouped.pop().unwrap());
			let qualified  = group[0].qualified;
			let unqualified  = group[0].unqualified;
			let category = group[0].bindee.category();

			while grouped.len() > 0 && grouped.last().unwrap().unqualified == unqualified {
				group.push_front(grouped.pop().unwrap());
			}

			//check that all of the RawBindings belong to the same category
			for i in 0 .. group.len() - 1 {
				ensure!(group[i].bindee.category() == category, "the name {} is bound to class \
				        clauses of different types", unqualified);
			}

			//branch based on the category
			match category {
				Category::FieldLike => {
					//check that the same state is not bound twice
					let mut seen_states = 0u32;
					for binding in &group {
						if (seen_states & (1 << binding.state_i as u32)) != 0 {
							bail!("duplicate field/const {} in the {} state", 
							      binding.qualified, binding.state_name);
						}

						seen_states |= 1 << binding.state_i as u32;
					}

					//because we used a stable sort above, our RawBindings are already in the
					//correct shadowing order (the right side of the vec shadows the left side).
					//we just branch on emitting a SimpleField, SimpleConst or StackableField.
					if group.len() > 1 {
						let field_stack_start = self.field_stack.len() as u16;

						for raw_binding in group.iter().rev() {
							let binding = self.bind_field_like(raw_binding)?;

							ensure!(self.bindings.insert(
								raw_binding.qualified,
								binding.clone()).is_none()
							);

							self.field_stack.push(binding.to_field_stack_entry());
						}

						self.field_stack.push(FieldStackEntry::End);

						ensure!(self.field_stack.len() <= u16::MAX as usize, 
						        "too many shadowing fields/consts");

						ensure!(self.bindings.insert(
							unqualified,
							Binding::StackableField(field_stack_start)
						).is_none());
					} else {
						let binding = self.bind_field_like(&group[0])?;
						ensure!(self.bindings.insert(unqualified, binding.clone()).is_none());
						ensure!(self.bindings.insert(qualified, binding).is_none());
					}
				}

				Category::MethLike => {
					//use stack_meths to validate and reorder the group of RawBindings
					let stacked = self.stack_meths(&group)?;

					if stacked.len() > 1 {
						//bind the method stack
						let meth_stack_start = self.meth_stack.len() as u16;
						for raw_binding in stacked.iter().rev() {
							ensure!(self.meth_stack.len() <= u16::MAX as usize, 
						            "too many wrapper methods");
							let stack_i = self.meth_stack.len() as u16;

							if !matches!(raw_binding.bindee, RawBindee::WildcardWrap(_)) {
								ensure!(self.bindings.insert(
									raw_binding.qualified,
									Binding::Meth(MethBinding::Stackable(
										raw_binding.state_i,
										stack_i
									))
								).is_none());
							}
							
							let binding = self.bind_meth_like(raw_binding);
							let stack_entry = binding.to_meth_stack_entry();

							self.meth_stack.push(stack_entry);
						}

						self.meth_stack.push(MethStackEntry::End);

						ensure!(self.meth_stack.len() <= u16::MAX as usize, 
						        "too many wrapper methods");

						ensure!(self.bindings.insert(
							unqualified,
							Binding::Meth(MethBinding::Stackable(0, meth_stack_start))
						).is_none());
					} else {
						let binding = self.bind_meth_like(&stacked[0]);

						ensure!(self.bindings.insert(unqualified, binding.clone()).is_none());
						ensure!(self.bindings.insert(qualified, binding).is_none());
					}
				}

				Category::PropLike => {
					//basically identical to our MethLike branch above, except we emit 
					//Binding::Props rather than Binding::Meths, and we check in advance that
					//all of the props have the same getter/setter arrangement (e.g. can't
					//wrap a getter-only property with a getter-and-setter property).
					let stacked = self.stack_meths(&group)?;

					if stacked.len() > 1 {
						let bindings = SmallVec::<[Binding; 8]>::from_iter(stacked.iter().rev()
							.map(|raw_binding| self.bind_prop_like(raw_binding)));

						let (has_getter, has_setter) = match &bindings[0] {
							Binding::Prop(ref get, ref set) => (get.is_some(), set.is_some()),
							_ => unreachable!()
						};

						for i in 1 .. bindings.len() {
							match bindings[i] {
								Binding::Prop(ref get, ref set) => {
									ensure!(has_getter == get.is_some() &&
									        has_setter == set.is_some(),
									        "getter/setter mismatch between wrap-prop {} and \
									        prop {}", stacked[i].qualified, stacked[0].qualified);
								}
								_ => unreachable!()
							}
						}

						//first produce a method stack for the getters...
						let getter_binding = if has_getter {
							let meth_stack_start = self.meth_stack.len() as u16;
							for binding in bindings.iter() {
								ensure!(self.meth_stack.len() <= u16::MAX as usize, 
							            "too many wrapper methods");

								let stack_entry = match *binding {
									Binding::Prop(ref get, _) => {
										get.as_ref().unwrap().to_meth_stack_entry()
									}
									_ => unreachable!()
								};
								self.meth_stack.push(stack_entry);
							}

							self.meth_stack.push(MethStackEntry::End);

							Some(MethBinding::Stackable(0, meth_stack_start))
						} else {
							None
						};

						//...then produce another stack for the setters...
						let setter_binding = if has_setter {
							let meth_stack_start = self.meth_stack.len() as u16;
							for binding in bindings.iter() {
								ensure!(self.meth_stack.len() <= u16::MAX as usize, 
							            "too many wrapper methods");

								let stack_entry = match *binding {
									Binding::Prop(_, ref set) => {
										set.as_ref().unwrap().to_meth_stack_entry()
									}
									_ => unreachable!()
								};
								self.meth_stack.push(stack_entry);
							}

							self.meth_stack.push(MethStackEntry::End);

							Some(MethBinding::Stackable(0, meth_stack_start))
						} else {
							None
						};

						ensure!(self.meth_stack.len() <= u16::MAX as usize, 
						        "too many wrapper methods");

						//...then finally, emit qualified bindings for each raw binding
						for (i, raw_binding) in stacked.iter().rev().enumerate() {
							if !matches!(raw_binding.bindee, RawBindee::WildcardWrapProp(..)) {
								let getter_meth_binding = match getter_binding {
									Some(MethBinding::Stackable(_, stack_start)) => {
										Some(MethBinding::Stackable(
											raw_binding.state_i,
											stack_start + i as u16
										))
									}
									None => None,
									_ => unreachable!()
								};

								let setter_meth_binding = match setter_binding {
									Some(MethBinding::Stackable(_, stack_start)) => {
										Some(MethBinding::Stackable(
											raw_binding.state_i,
											stack_start + i as u16
										))
									}
									None => None,
									_ => unreachable!()
								};
							
								ensure!(self.bindings.insert(
									raw_binding.qualified,
									Binding::Prop(
										getter_meth_binding,
										setter_meth_binding
									)
								).is_none());
							}
						}

						//emit the unqualified binding
						ensure!(self.bindings.insert(
							unqualified,
							Binding::Prop(getter_binding, setter_binding)
						).is_none());
					} else {
						let binding = self.bind_prop_like(&stacked[0]);

						ensure!(self.bindings.insert(unqualified, binding.clone()).is_none());
						ensure!(self.bindings.insert(qualified, binding).is_none());
					}
				}
			}
		}

		Ok(self.into_class())
	}

	fn bind_field_like(&mut self, raw_binding: &RawBinding) -> GResult<Binding> {
		let state_i = raw_binding.state_i;
		match raw_binding.bindee {
			RawBindee::Field => {
				ensure!(self.field_count < u16::MAX, "class has too many fields");
				self.field_count += 1;
				Ok(Binding::SimpleField(state_i, self.field_count as u16 - 1))
			}
			RawBindee::Const(ref slot) => {
				Ok(Binding::SimpleConst(state_i, slot.clone()))
			}
			_ => unreachable!()
		}
	}

	fn bind_meth_like(&mut self, raw_binding: &RawBinding) -> Binding {
		let state_i = raw_binding.state_i;

		let (gfn, rni) = match raw_binding.bindee {
			RawBindee::Meth(ref gfn) => (gfn.clone(), false),
			RawBindee::Wrap(_, ref gfn) => (gfn.clone(), true),
			RawBindee::WildcardWrap(ref gfn) => (gfn.clone(), true),
			_ => unreachable!()
		};

		Binding::Meth(MethBinding::Simple(state_i, gfn.clone(), rni))
	}

	fn bind_prop_like(&mut self, raw_binding: &RawBinding) -> Binding {
		let state_i = raw_binding.state_i;
		
		let (_qualified_get, _qualified_set, getter_gfn, setter_gfn, rni) = {
			match raw_binding.bindee {
				RawBindee::Prop(qg, qs, ref get, ref set) => {
					(qg, qs, get.clone(), set.clone(), false)
				}
				RawBindee::WrapProp(qg, qs, _, ref get, ref set) => {
					(qg, qs, get.clone(), set.clone(), true)
				}
				RawBindee::WildcardWrapProp(qg, qs, ref get, ref set) => {
					(qg, qs, get.clone(), set.clone(), true)
				}
				_ => unreachable!()
			}
		};

		let getter_binding = getter_gfn.as_ref().map(|getter_gfn| {
			MethBinding::Simple(state_i, getter_gfn.clone(), rni)
		});

		let setter_binding = setter_gfn.as_ref().map(|setter_gfn| {
			MethBinding::Simple(state_i, setter_gfn.clone(), rni)
		});

		Binding::Prop(getter_binding, setter_binding)
	}

	fn stack_meths(
		&mut self,
		raw_bindings: &VecDeque<RawBinding>
	) -> GResult<Vec<RawBinding>> {

		/*
		the input is a slice of MethLike or PropLike RawBindings, in field-shadowing order: fields
		towards the right of the slice shadow those towards the left.

		the output is those same RawBindings, reordered so that they can be bound as a method
		stack or property stack. any number of meths/props, followed by explicit wraps/wrap-props 
		in their explicit order, followed by wildcard wraps/wrap-props in field-shadowing order.

		we also detect, and store, dependencies between states implied by the explicit wraps,
		mutating their `requires`, `required_by` and `excludes` bitflags.
		*/

		let mut src = Vec::from_iter(raw_bindings.iter().cloned());
		let mut dst = Vec::<RawBinding>::with_capacity(src.len());

		//drain all of the meths from src and put them at the start of dst. set the `excludes`
		//flag for any two states which both share a meth.
		dst.extend(src.drain_filter(|rb| {
			matches!(rb.bindee, RawBindee::Meth(_) | RawBindee::Prop(..))
		}));

		for i in 0 .. dst.len() {
			for j in i + 1 .. dst.len() {
				let state_i = dst[i].state_i;
				let state_j = dst[j].state_i;
				ensure!(state_i != state_j, "duplicate `meth` form {}", dst[i].qualified);

				self.states.get_mut(&dst[i].state_name)
					.unwrap().excludes |= 1 << (state_j as u32);
				self.states.get_mut(&dst[j].state_name)
					.unwrap().excludes |= 1 << (state_i as u32);
			}
		}

		//starting from the first meth in dst (if any), search src for any wraps which target that
		//meth's qualified name, and move them to dst. set the `requires` flag for each wrap's 
		//state, and the `excludes` flag for any state which wraps the same qualified name. 
		//continue scanning through until you've checked every meth/wrap in `dst`.
		let mut dst_i = 0;
		while dst_i < dst.len() {
			let dst_qualified = dst[dst_i].qualified;
			let dst_state = dst[dst_i].state_name;
			let dst_state_i = dst[dst_i].state_i;

			let starting_len = dst.len();

			//this is O(n^2) but n will usually be very small
			dst.extend(src.drain_filter(|src_rb| {
				match src_rb.bindee {
					RawBindee::Meth(_) | RawBindee::Prop(..) => unreachable!(),
					RawBindee::Wrap(target_qualified, _) |
					RawBindee::WrapProp(_, _, target_qualified, _, _) => {
						if target_qualified == dst_qualified {
							let src_state = src_rb.state_name;
							let src_state_i = src_rb.state_i;

							if src_state_i != dst_state_i {
								self.states.get_mut(&src_state)
									.unwrap().requires |= 1 << (dst_state_i as u32);
								self.states.get_mut(&dst_state)
									.unwrap().required_by |= 1 << (src_state_i as u32);
							}

							true
						} else {
							false
						}
					}
					RawBindee::WildcardWrap(_) | RawBindee::WildcardWrapProp(..) => false,
					_ => unreachable!()
				}
			}));

			//all entries in `dst` from `starting_len` onwards wrap the same name, so they all
			//need to exclude one another
			for i in starting_len .. dst.len() {
				for j in i + 1 .. dst.len() {
					let state_i = dst[i].state_i;
					let state_j = dst[j].state_i;
					ensure!(state_i != state_j, "duplicate `wrap` form {}", dst[i].qualified);

					self.states.get_mut(&dst[i].state_name)
						.unwrap().excludes |= 1 << (state_j as u32);
					self.states.get_mut(&dst[j].state_name)
						.unwrap().excludes |= 1 << (state_i as u32);
				}
			}

			dst_i += 1;
		}

		//if any non-wildcard wraps remain in `src`, they must either refer to a name which
		//doesn't exist, or refer to other wraps in a cycle. throw an error.
		for rb in &src {
			match rb.bindee {
				RawBindee::Meth(_) | RawBindee::Prop(..) => unreachable!(),
				RawBindee::Wrap(target, _) => {
					bail!("the wrapped method {} is nonexistent or cyclical", target)
				}
				RawBindee::WrapProp(_, _, target, _, _) => {
					bail!("the wrapped property {} is nonexistent or cyclical", target)
				}
				RawBindee::WildcardWrap(_) | RawBindee::WildcardWrapProp(..) => (),
				_ => unreachable!()
			}
		}

		//move all of the wildcard wraps to the end of the stack and return it.
		dst.extend(src.into_iter());
		Ok(dst)
	}

	fn evaluate_consts(&mut self) -> GResult<()> {
		/*
		const-evaluation is pretty finicky, because consts need to be able to refer to all
		previous consts in the current state (including mixins!), as well as all of the consts in
		each of their parent states. the flattened set of RawBindings is unhelpful for this.

		we traverse the state hierarchy, starting from Main. for each state, we maintain a table
		which contains all of its existing consts (plus @class-name and @state-name), and pass
		that table in to each const-initialization fn which refers to another const (which the
		(class) macro will have syntactically transformed into [table-arg 'const-name]).

		we need to allocate a fresh table for each const, because it might be captured by a
		(fn) constant, in which case it could persist indefinitely. in the future (todo), we
		should only populate the table with those names which the const will actually access.
		*/

		let const_tab = glsp::tab();
		const_tab.set(CLASS_NAME_SYM, match self.raw_class.name {
			Some(class_name) => Val::Sym(class_name),
			None => Val::Nil
		})?;

		self.evaluate_consts_for_state(MAIN_SYM, const_tab)
	}

	fn evaluate_consts_for_state(
		&mut self, 
		state_name: Sym,
		parent_consts: Root<Tab>
	) -> GResult<()> {

		let own_consts = parent_consts.shallow_clone();
		own_consts.set(STATE_NAME_SYM, state_name)?;

		//evaluate each of this state's PreConst bindings, converting them into Consts. the
		//initializer-gfn returns an array (a b c), where a and b are the two PreConsts before
		//the current PreConst, each of which must not have had an initializer-gfn. (this is
		//a slightly wacky serialization, but it's the easiest way to support patterns.)
		let mut i = 0;
		while i < self.raw_class.bindings.len() {
			if matches!(self.raw_class.bindings[i].bindee, RawBindee::PreConst(_)) &&
			   self.raw_class.bindings[i].state_name == state_name {
				let mut j = i;
				while j < self.raw_class.bindings.len() &&
				      matches!(self.raw_class.bindings[j].bindee, RawBindee::PreConst(None)) {
					j += 1;
				}

				assert!(j < self.raw_class.bindings.len());
				let gfn = match self.raw_class.bindings[j].bindee {
					RawBindee::PreConst(Some(ref gfn)) => gfn.clone(),
					_ => unreachable!()
				};

				//evaluate the const initializer. todo: we're currently very gc-unsafe here... 
				//nothing is rooted. if a const initializer triggers a gc, we're in trouble.
				let results: Root<Arr> = if gfn.min_args() == 0 {
					glsp::call(&gfn, &())?
				} else {
					let const_tab = own_consts.shallow_clone();
					const_tab.freeze();
					glsp::call(&gfn, &[const_tab])?
				};

				//assign the results of the initializer to each const from i to j inclusive
				assert!(results.len() == (j - i) + 1);
				for n in i ..= j {
					let result: Val = results.get(n - i)?;
					let raw_binding = &mut self.raw_class.bindings[n];

					assert!(raw_binding.state_name == state_name);
					own_consts.set(raw_binding.unqualified, &result)?;
					own_consts.set(raw_binding.qualified, &result)?;

					raw_binding.bindee = RawBindee::Const(Slot::from_val(&result));
				}

				//continue to iterate
				i = j + 1;
			} else {
				i += 1;
			}
		}

		//recurse for each child state
		for child in self.states[&state_name].children.clone() {
			self.evaluate_consts_for_state(child, own_consts.clone())?;
		}

		Ok(())
	}

	fn into_class(self) -> Class {
		Class {
			header: GcHeader::new(),
			name: self.raw_class.name,
			is_mixin: self.raw_class.is_mixin,

			bindings: self.bindings,
			field_count: self.field_count as usize,
			field_stack: self.field_stack,
			meth_stack: self.meth_stack,
			is: self.is,
			states: self.states,

			raw_class: if self.raw_class.is_mixin { 
				Some(self.raw_class)
			} else {
				None
			}
		}
	}
}

impl State {

	//a raw-state is a tab with the following fields:
	//	- name: a sym.
	//	- enabled-by-default?: a bool
	//	- parent: a sym; may be absent
	//	- children: an arr of syms
	//	- fsm-siblings: an arr of syms

	fn from_tab(tab: &Tab, state_i: u8) -> GResult<State> {
		let state_name: Sym = tab.get(NAME_SYM)?;
		let enabled_by_default: bool = tab.get(ENABLED_BY_DEFAULTP_SYM)?;
		let parent: Option<Sym> = tab.get_if_present(PARENT_SYM)?;
		let children: Vec<Sym> = tab.get(CHILDREN_SYM)?;
		let fsm_siblings: Vec<Sym> = tab.get(FSM_SIBLINGS_SYM)?;

		Ok(State {
			index: state_i,

			name: state_name,
			enabled_by_default,
			parent,
			children,
			fsm_siblings,

			requires: 0,
			required_by: 0,
			excludes: 0,

			init: None,
			finis: Vec::new()
		})
	}
}
