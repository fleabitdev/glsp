use std::{isize};
use std::cell::{Cell, RefCell};
use std::convert::{TryFrom};
use std::fmt::{self, Debug, Formatter};
use std::marker::{PhantomData};
use std::mem::{size_of};
use super::ast::{ParamList};
use super::engine::{glsp, Span, Sym, with_heap};
use super::error::{GError, GResult};
use super::gc::{Allocate, Raw, Header, Root, Slot, Visitor};
use super::transform::{Predicate};
use super::val::{Val};
use super::wrap::{CallableOps};

#[cfg(feature = "compiler")]
use serde::{Deserialize, Serialize};

#[doc(hidden)]
pub struct Bytecode {
	pub(crate) header: Header,
	pub(crate) instrs: Vec<Instr>,
	pub(crate) spans: Vec<Span>,
	pub(crate) start_regs: Vec<Slot>,
	pub(crate) start_stays: Vec<StaySource>,
	pub(crate) local_count: u8,
	pub(crate) scratch_count: u8,
	pub(crate) literal_count: u8,
	pub(crate) lambdas: Vec<Raw<Lambda>>,
	pub(crate) defers: Vec<usize>
}

#[derive(PartialEq)]
pub(crate) enum StaySource {
	//a None stay, which will later be populated by a MakeStay instr for some local variable
	Empty,

	//a Some(stay), created by immediately promoting a particular parameter local variable
	Param(u8),

	//a Some(stay), cloned from one of the captured stays of the gfn which is being called
	Captured(u8),

	//a Some(stay) which already existed at the time that this Bytecode was compiled. we
	//currently use this type of stay to support toplevel (let) forms
	PreExisting(Raw<Stay>)
}

impl Debug for StaySource {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			StaySource::Empty => write!(f, "Empty"),
			StaySource::Param(id) => write!(f, "Param({})", id),
			StaySource::Captured(id) => write!(f, "Captured({})", id),
			StaySource::PreExisting(_) => write!(f, "PreExisting(_)")
		}
	}
}

#[doc(hidden)]
pub struct Lambda {
	pub(crate) header: Header,
	
	pub(crate) bytecode: Raw<Bytecode>,
	pub(crate) param_map: ParamMap,
	pub(crate) name: Option<Sym>,
	pub(crate) yields: bool,
	
	//the stay references which are cloned from the immediately-enclosing stack frame when we 
	//create a new closure from this Lambda. the u8 indexes are the same as for the LoadStay
	//and SetStay instrs.
	pub(crate) captures: Vec<u8>
}

/**
The `fn` primitive type.

The name `GFn` was chosen to avoid a name collision with Rust's [`Fn` trait][0].

[0]: https://doc.rust-lang.org/std/ops/trait.Fn.html

Most of this type's methods belong to the `callable` abstract type, so they can be found in
the [`CallableOps`](trait.CallableOps.html) trait. To invoke a function, use
[`glsp::call`](fn.call.html).

GameLisp functions are always stored on the garbage-collected heap, so they're normally 
represented by the type [`Root<GFn>`](struct.Root.html).
*/

pub struct GFn {
	header: Header,
	pub(crate) lambda: Raw<Lambda>,
	pub(crate) captured_stays: Vec<Raw<Stay>>
}

impl GFn {
	pub(crate) fn new(lambda: &Raw<Lambda>, captured_stays: Vec<Raw<Stay>>) -> GFn {
		GFn {
			header: Header::new(),
			lambda: lambda.clone(),
			captured_stays
		}
	}

	/**
	Returns `true` if this function will allocate a coroutine when called.

	Equivalent to [`(fn-yields? f)`](https://gamelisp.rs/std/fn-yields-p).
	*/
	pub fn yields(&self) -> bool {
		self.lambda.yields
	}
}

//vm.rs currently needs to be able to copy its callee to a Slot, so we can't implement CallableOps
//for GFn or &GFn.
impl CallableOps for Root<GFn> {
	fn receive_call(&self, arg_count: usize) -> GResult<Val> {
		glsp::call_gfn(self, arg_count)
	}

	fn arg_limits(&self) -> (usize, Option<usize>) {
		let param_map = &self.lambda.param_map;
		(param_map.min_args, param_map.max_args)
	}

	fn name(&self) -> Option<Sym> {
		self.lambda.name
	}
}

impl CallableOps for Raw<GFn> {
	fn receive_call(&self, arg_count: usize) -> GResult<Val> {
		glsp::call_gfn(&self.root(), arg_count)
	}

	fn arg_limits(&self) -> (usize, Option<usize>) {
		let param_map = &self.lambda.param_map;
		(param_map.min_args, param_map.max_args)
	}

	fn name(&self) -> Option<Sym> {
		self.lambda.name
	}
}

//using Cell rather than RefCell here shrinks Stay by one word. it requires a small amount of
//juggling when accessing or tracing the stored Slot, but i'm hopeful that should be optimised 
//away, at least in "unsafe-internals" mode.
#[doc(hidden)]
pub struct Stay {
	header: Header,
	slot: Cell<Slot>
}

impl Stay {
	pub(crate) fn new(slot: Slot) -> Stay {
		Stay {
			header: Header::new(),
			slot: Cell::new(slot)
		}
	}

	pub(crate) fn get(&self) -> Slot {
		let slot = self.slot.replace(Slot::Nil);
		let result = slot.clone();
		self.slot.set(slot);
		result
	}

	pub(crate) fn set(&self, new_slot: Slot) {
		with_heap(|heap| heap.write_barrier_slot(self, &new_slot));
		self.slot.set(new_slot);
	}
}

/**
The `coro` primitive type.

Coroutines can't be instantiated directly. Instead, use [`glsp::call`](fn.call.html) to invoke
a [`GFn`](struct.GFn.html) for which [`GFn::yields`](struct.GFn.html#method.yields) returns
`true`.

To resume a `newborn` or `paused` coroutine, use [`glsp::coro_run`](fn.coro_run.html).

Coroutines are always stored on the garbage-collected heap, so they're normally represented by
the type [`Root<Coro>`](struct.Root.html).
*/

pub struct Coro {
	header: Header,
	pub(crate) state: Cell<PrivCoroState>,
	pub(crate) storage: RefCell<CoroStorage>
}

/**
The return value for the [`Coro::state`](struct.Coro.html#method.state) method.
*/

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CoroState {
	Newborn,
	Running,
	Paused,
	Finished,
	Poisoned
}

#[derive(Copy, Clone)]
pub(crate) enum PrivCoroState {
	Newborn,
	Running,
	Paused(u8), //the u8 is the resume_reg
	Finished,
	Poisoned,
	Recycling
}

pub(crate) struct CoroStorage {
	//this is only None in the Recycling state
	pub(crate) gfn: Option<Raw<GFn>>,

	pub(crate) instr: usize,

	//in any state other than Newborn or Paused, these Vecs are present but empty.
	pub(crate) regs: Vec<Slot>,
	pub(crate) stays: Vec<Option<Raw<Stay>>>,
	pub(crate) defers: Vec<usize>
}

impl Coro {
	pub(crate) fn new(
		gfn: Raw<GFn>, 
		regs: Vec<Slot>, 
		stays: Vec<Option<Raw<Stay>>>
	) -> Coro {
		assert!(gfn.yields());

		Coro {
			header: Header::new(),
			state: Cell::new(PrivCoroState::Newborn),
			storage: RefCell::new(CoroStorage {
				gfn: Some(gfn),
				instr: 0,
				regs,
				stays,
				defers: Vec::new()
			})
		}
	}

	//called after creating or pausing a Coro - these are the only times that its content might
	//be mutated in a way which doesn't leave it empty
	pub(crate) fn write_barrier(&self) {
		with_heap(|heap| {
			let storage = self.storage.borrow();

			if let Some(ref gfn) = storage.gfn {
				heap.write_barrier(self, gfn);
			}

			for reg in &storage.regs {
				heap.write_barrier_slot(self, reg);
			}

			for stay in &storage.stays {
				if let Some(ref stay) = *stay {
					heap.write_barrier(self, stay);
				}
			}
		})
	}

	///Returns the `GFn` from which this coroutine originated.
	pub fn gfn(&self) -> Root<GFn> {
		self.storage.borrow().gfn.as_ref().unwrap().root()
	}

	/**
	Returns the coroutine's current state.

	Equivalent to [`(coro-state co)`](https://gamelisp.rs/std/coro-state).
	*/
	pub fn state(&self) -> CoroState {
		match self.state.get() {
			PrivCoroState::Newborn => CoroState::Newborn,
			PrivCoroState::Running => CoroState::Running,
			PrivCoroState::Paused(_) => CoroState::Paused,
			PrivCoroState::Finished => CoroState::Finished,
			PrivCoroState::Poisoned => CoroState::Poisoned,
			PrivCoroState::Recycling => unreachable!()
		}
	}
}

impl Allocate for Bytecode {
	fn header(&self) -> &Header {
		&self.header
	}
	
	fn visit_raws<V: Visitor>(&self, visitor: &mut V) {
		let local_end_i = self.local_count as usize;
		for local_init in &self.start_regs[..local_end_i] {
			visitor.visit_slot(local_init);
		}

		let literal_start_i = (self.local_count + self.scratch_count) as usize;
		for literal in &self.start_regs[literal_start_i..] {
			visitor.visit_slot(literal);
		}

		debug_assert!(self.start_regs[local_end_i..literal_start_i].iter().all(|slot| {
			*slot == Slot::Nil
		}));

		for stay_source in &self.start_stays {
			if let StaySource::PreExisting(stay) = stay_source {
				visitor.visit_raw(stay);
			}
		}

		for lambda in &self.lambdas {
			visitor.visit_raw(lambda);
		}
	}

	fn clear_raws(&self) {
		//deliberate no-op
	}

	fn owned_memory_usage(&self) -> usize {
		self.instrs.capacity() * size_of::<Instr>()
		+ self.spans.capacity() * size_of::<Span>()
		+ self.start_regs.capacity() * size_of::<Slot>()
		+ self.start_stays.capacity() * size_of::<StaySource>()
		+ self.lambdas.capacity() * size_of::<Raw<Lambda>>()
		+ self.defers.capacity() * size_of::<usize>()
	}
}

impl Allocate for Lambda {
	fn header(&self) -> &Header {
		&self.header
	}
	
	fn visit_raws<V: Visitor>(&self, visitor: &mut V) {
		visitor.visit_raw(&self.bytecode);
	}

	fn clear_raws(&self) {
		//deliberate no-op
	}

	fn owned_memory_usage(&self) -> usize {
		self.captures.capacity()
	}
}

impl Allocate for GFn {
	fn header(&self) -> &Header {
		&self.header
	}

	fn visit_raws<V: Visitor>(&self, visitor: &mut V) {
		visitor.visit_raw(&self.lambda);
		for stay in &self.captured_stays {
			visitor.visit_raw(stay)
		}
	}

	fn clear_raws(&self) {
		//deliberate no-op
	}

	fn owned_memory_usage(&self) -> usize {
		self.captured_stays.capacity() * size_of::<Raw<Stay>>()
	}
}

impl Allocate for Stay {
	fn header(&self) -> &Header {
		&self.header
	}
	
	fn visit_raws<V: Visitor>(&self, visitor: &mut V) {
		visitor.visit_slot(&self.get());
	}

	fn clear_raws(&self) {
		self.slot.set(Slot::Nil);
	}

	fn owned_memory_usage(&self) -> usize {
		0
	}
}

impl Allocate for Coro {
	fn header(&self) -> &Header {
		&self.header
	}
	
	fn visit_raws<V: Visitor>(&self, visitor: &mut V) {
		let storage = self.storage.borrow();

		if let Some(ref gfn) = storage.gfn {
			visitor.visit_raw(gfn);
		}

		for slot in &storage.regs {
			visitor.visit_slot(slot);
		}

		for stay in &storage.stays {
			if let Some(ref stay) = *stay {
				visitor.visit_raw(stay);
			}
		}
	}

	fn clear_raws(&self) {
		self.state.set(PrivCoroState::Recycling);

		let mut storage = self.storage.borrow_mut();
		storage.gfn = None;
		storage.regs.clear();
		storage.stays.clear();
		storage.defers.clear();
	}

	fn owned_memory_usage(&self) -> usize {
		let storage = self.storage.borrow();

		storage.regs.capacity() * size_of::<Slot>() +
		storage.stays.capacity() * size_of::<Raw<Stay>>() +
		storage.defers.capacity() * size_of::<usize>()
	}
}


//-------------------------------------------------------------------------------------------------
// ParamMap
//-------------------------------------------------------------------------------------------------

#[cfg_attr(feature = "compiler", derive(Clone, Deserialize, Serialize))]
pub(crate) struct ParamMap {
	pub(crate) param_count: usize,
	pub(crate) basic_param_count: usize,
	pub(crate) opt_param_count: usize,

	//None for no param, Some(false) for .._, Some(true) for ..name
	pub(crate) rest_param: Option<bool>,

	min_args: usize,
	max_args: Option<usize>
}

impl ParamMap {
	pub(crate) fn from_param_list(
		param_list: &ParamList,
		arg_limits: &Option<(usize, Option<usize>)>,
		span: Span,
	) -> GResult<ParamMap> {

		let param_count = param_list.param_count();
		let basic_param_count = param_list.basic_params.len();
		let opt_param_count = param_list.opt_params.len();
		let rest_param = param_list.rest_param.map(|o| o.is_some());

		let (min_args, max_args) = match arg_limits {
			Some(pair) => {
				ensure_at!(span, basic_param_count == 0 && opt_param_count == 0 && 
				           rest_param.is_some(), "the &arg-limits flag is only valid with \
				           a param list of (..x) or (.._)");
				*pair
			}
			None => {
				let min_args = basic_param_count;
				let max_args = if rest_param.is_some() {
					None
				} else {
					Some(basic_param_count + opt_param_count)
				};

				(min_args, max_args)
			}
		};

		Ok(ParamMap {
			param_count,
			basic_param_count,
			opt_param_count,
			rest_param,

			min_args,
			max_args
		})
	}
	
	/*
	the calling convention: 

	the first local registers of each stack frame have a one-to-one relationship with each of the
	fn's named params, including (?) and ..rest params. the caller pushes all of their arguments
	onto the stack and passes in an argument count to the callee. the callee pushes initializer 
	values for any missing (?) arguments, or pops any extra .. arguments, potentially collating 
	them into an array. the caller then pushes any local, scratch or literal registers onto the 
	stack.

	when a param is also a stay, it still occupies a "dummy" register which is never actually
	accessed by the generated code. when stays are pushed onto the stay stack for this stack
	frame, one of the possible sources for a stay is to promote one of the argument registers
	from the stack onto the heap, replacing its register value with #n.
	*/
	
	#[inline]
	pub(crate) fn wrangle_args(
		&self, 
		regs: &mut Vec<Slot>,
		start_regs: &[Slot],
		arg_count: usize, 
		callsite: Option<Span>
	) -> Result<(), String> {

		if arg_count < self.min_args {
			return Err(format!("received {} argument{}, but expected at least {}",
			                   arg_count, if arg_count == 1 { "" } else { "s" }, self.min_args))
		}

		if let Some(max_args) = self.max_args {
			if arg_count > max_args {
				return Err(format!("received {} argument{}, but expected no more than {}",
				                   arg_count, if arg_count == 1 { "" } else { "s" },
				                   max_args))
			}
		}

		let non_rest_param_count = self.basic_param_count + self.opt_param_count;
		
		if arg_count < non_rest_param_count {
			regs.extend_from_slice(&start_regs[arg_count .. non_rest_param_count]);
		}

		if let Some(to_collect) = self.rest_param {
			let rest_arg_count = arg_count.saturating_sub(non_rest_param_count);
			let rest_base_index = regs.len() - rest_arg_count;

			if to_collect {
				let arr = with_heap(|heap| {
					heap.recycler.arr_from_iter(
						regs.drain(rest_base_index..)
					).unwrap()
				});
				arr.set_span(glsp::new_arr_span(callsite));

				regs.push(Slot::Arr(arr.into_raw()));
			} else {
				regs.truncate(rest_base_index);
			}
		}
		
		Ok(())
	}
}


//-------------------------------------------------------------------------------------------------
// Instr
//-------------------------------------------------------------------------------------------------

//i considered a variable-length encoding for instructions, but the cost of safely decoding it
//would probably swamp any performance savings from being a little more cache-efficient.
//instead we use a fixed-length encoding. the 8-byte alignment was chosen because it led to
//a modest performance improvement in the interpreter, compared to the default 1-byte alignment.
#[derive(Clone, Copy)]
#[cfg_attr(feature = "compiler", derive(Deserialize, Serialize))]
#[repr(align(8))]
pub(crate) enum Instr {
	
	//if you add or remove registers from any variant, don't forget to change the register_mut
	//method at the bottom of this file! you'll also need to amend print.rs, vm.rs and encoder.rs.
	CopyRegister(u8, u8),
	LoadGlobal(u8, SymBytes),
	SetGlobal(u8, SymBytes),
	LoadStay(u8, u8),
	SetStay(u8, u8),
	MakeStay(u8, u8),
	MakeGFn(u8, u8),
	Call0(u8, u8),
	Call1(u8, u8, u8),
	Call2(u8, u8, u8, u8),
	CallN(u8, u8, u8),
	Splay([u8; 4]),
	Return(u8),
	Yield(u8, u8),
	Jump(JumpBytes),
	JumpIfTrue(u8, JumpBytes),
	JumpIfFalse(u8, JumpBytes),
	PushDefer(u8),
	RunAndPopDefers(u8),
	RunDefer(u8),
	EndDefer(),

	OpAdd(u8, u8, u8),
	OpSub(u8, u8, u8),
	OpMul(u8, u8, u8),
	OpDiv(u8, u8, u8),
	OpRem(u8, u8, u8),
	OpAbs(u8, u8),
	OpNeg(u8, u8),
	OpSign(u8, u8),
	OpMin(u8, u8, u8),
	OpMax(u8, u8, u8),
	OpPredicate(u8, u8, Predicate),
	OpInt(u8, u8),
	OpFlo(u8, u8),
	OpBool(u8, u8),
	OpNumEq(u8, u8, u8),
	OpLt(u8, u8, u8),
	OpLte(u8, u8, u8),
	OpGt(u8, u8, u8),
	OpGte(u8, u8, u8),
	OpNot(u8, u8),
	OpIter(u8, u8),
	OpIterNext(u8, u8),
	OpIterNextBack(u8, u8),
	OpIterFinishedp(u8, u8),
	OpLen(u8, u8),
	OpHasp(u8, u8, u8),
	OpAccess(u8, u8, u8),
	OpSetAccess(u8, u8, u8, u8),
	OpArr(u8, u8, u8),
	OpCallMet(u8, u8, u8),
	OpCallMetOpt(u8, u8, u8),
	OpCallBaseRaw(u8, u8, u8),
	OpGlobal(u8, u8),
	OpSetGlobal(u8, u8, u8)
}

//SymBytes and JumpBytes are encoded representations of Sym and isize respectively. they have
//one-byte alignment and they are as compact as possible.

#[derive(Copy, Clone)]
#[cfg_attr(feature = "compiler", derive(Deserialize, Serialize), serde(from = "Sym", into = "Sym"))]
pub(crate) struct SymBytes([u8; 3]);

impl From<Sym> for SymBytes {
	#[inline]
	fn from(src: Sym) -> SymBytes {
		let bytes = src.0.to_le_bytes();
		assert!(bytes[3] == 0x00);
		SymBytes([bytes[0], bytes[1], bytes[2]])
	}
}

impl From<SymBytes> for Sym {
	#[inline]
	fn from(src: SymBytes) -> Sym {
		let bytes = src.0;
		Sym(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], 0x00]), PhantomData)
	}
}

#[derive(Copy, Clone)]
#[cfg_attr(feature = "compiler", derive(Deserialize, Serialize))]
pub struct JumpBytes([u8; 3]);

impl TryFrom<isize> for JumpBytes {
	type Error = GError;

	#[inline]
	fn try_from(src: isize) -> GResult<JumpBytes> {
		if src < -8_388_608 || src > 8_388_607 {
			panic!("attempted to construct a JumpBytes from an out-of-range value")
		} else {
			let bytes = src.to_le_bytes();
			debug_assert!({
				let sign_extend = if bytes[2] & 0x80 == 0x80 { 0xffu8 } else { 0x00 };
				bytes[3..].iter().all(|&b| b == sign_extend)
			});

			Ok(JumpBytes([bytes[0], bytes[1], bytes[2]]))
		}
	}
}

impl From<JumpBytes> for isize {
	#[inline]
	fn from(src: JumpBytes) -> isize {
		let bytes = src.0;
		let sign_extend = if bytes[2] & 0x80 == 0x80 { 0xffu8 } else { 0x00 };
		i32::from_le_bytes([bytes[0], bytes[1], bytes[2], sign_extend]) as isize
	}
}

//the encoder often needs to emit "placeholder" register or jump values, then go back and adjust 
//them later on. to facilitate this without using raw pointers, we allow Instrs' register values to 
//be indexed dynamically.
macro_rules! indexing_match {
	($match_expr:expr, $index_expr:expr,
	 $($variant_name:ident($($reg_name:ident $reg_index:literal),*)),+) => (
		match $match_expr {
			$(
				$variant_name($(ref mut $reg_name, )* ..) => {
					$(if $index_expr == $reg_index {
						$reg_name
					} else)* {
						panic!()
					}
				}
			),+
		}
	);
}

impl Instr {
	pub fn register_mut(&mut self, index: usize) -> &mut u8 {
		use Instr::*;

		indexing_match!(*self, index,
			CopyRegister(a 0, b 1),
			LoadGlobal(a 0),
			SetGlobal(a 0),
			LoadStay(a 0, b 1),
			SetStay(a 0, b 1),
			MakeStay(a 0, b 1),
			MakeGFn(a 0, b 1),
			Call0(a 0, b 1),
			Call1(a 0, b 1, c 2),
			Call2(a 0, b 1, c 2, d 3),
			CallN(a 0, b 1, c 2),
			Splay(),
			Return(a 0),
			Yield(a 0, b 1),
			Jump(),
			JumpIfTrue(a 0),
			JumpIfFalse(a 0),
			PushDefer(),
			RunAndPopDefers(),
			RunDefer(),
			EndDefer(),

			OpAdd(a 0, b 1, c 2),
			OpSub(a 0, b 1, c 2),
			OpMul(a 0, b 1, c 2),
			OpDiv(a 0, b 1, c 2),
			OpRem(a 0, b 1, c 2),
			OpAbs(a 0, b 1),
			OpNeg(a 0, b 1),
			OpSign(a 0, b 1),
			OpMin(a 0, b 1, c 2),
			OpMax(a 0, b 1, c 2),
			OpPredicate(a 0, b 1),
			OpInt(a 0, b 1),
			OpFlo(a 0, b 1),
			OpBool(a 0, b 1),
			OpNumEq(a 0, b 1, c 2),
			OpLt(a 0, b 1, c 2),
			OpLte(a 0, b 1, c 2),
			OpGt(a 0, b 1, c 2),
			OpGte(a 0, b 1, c 2),
			OpNot(a 0, b 1),
			OpIter(a 0, b 1),
			OpIterNext(a 0, b 1),
			OpIterNextBack(a 0, b 1),
			OpIterFinishedp(a 0, b 1),
			OpLen(a 0, b 1),
			OpHasp(a 0, b 1, c 2),
			OpAccess(a 0, b 1, c 2),
			OpSetAccess(a 0, b 1, c 2, d 3),
			OpArr(a 0, b 1, c 2),
			OpCallMet(a 0, b 1, c 2),
			OpCallMetOpt(a 0, b 1, c 2),
			OpCallBaseRaw(a 0, b 1, c 2),
			OpGlobal(a 0, b 1),
			OpSetGlobal(a 0, b 1, c 2)
		)
	}

	pub fn replace_jump_bytes(&mut self, new_bytes: JumpBytes) {
		use Instr::*;

		match self {
			Jump(ref mut bytes) => *bytes = new_bytes,
			JumpIfTrue(_, ref mut bytes) => *bytes = new_bytes,
			JumpIfFalse(_, ref mut bytes) => *bytes = new_bytes,
			_ => panic!()
		}
	}
}
