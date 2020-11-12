use smallvec::{SmallVec};
use std::{i32, f32, fmt};
use std::cell::{Cell, RefCell, RefMut};
use std::cmp::{Ordering};
use std::convert::{From};
use std::iter::{FromIterator};
use std::mem::{forget, replace};
use super::class::{Class, Obj};
use super::code::{Bytecode, Coro, GFn, Instr, PrivCoroState, Stay, StaySource};
use super::collections::{Arr, DequeAccess, DequeOps, Str};
use super::engine::{
	Filename, glsp, Guard, RData, Span, SpanStorage::Expanded, 
	stock_syms::*, Sym, with_heap
};
use super::error::{GError, GResult};
use super::gc::{Allocate, Gc, Slot, Root};
use super::iter::{GIterLen, IterableOps};
use super::transform::{Predicate};
use super::val::{Val, float_total_eq, float_total_cmp};
use super::wrap::{CallableOps};


//-------------------------------------------------------------------------------------------------
// Vm
//-------------------------------------------------------------------------------------------------

/*
the rust callstack and the glsp callstack are one and the same - each new gfn/rfn/coro invocation
is a new call to interpret(). in order to prevent stack overflows (which are safe, but would cause
the process to abort), we enforce a hard glsp recursion limit, currently 256. the overhead of a 
single non-inlined rust function call is about 7ns - a fraction of the total cost of a gfn call.

because rust doesn't support alloca, we also need to maintain parallel stacks (one Vec each) to 
store registers, stays, and backtrace information. the register stack doubles up as a temporary
scratch space for call() arguments.

because glsp code can invoke arbitrary rust code (rfns), and rust code can initiate an operation
which needs immutable access to the register/stay stacks (garbage collection), we take care to
temporarily release any mutable borrow of those stacks when calling an rfn.
*/

pub(crate) struct Vm {
	pub(crate) stacks: RefCell<Stacks>,

	//frames are guarded by a separate RefCell which we only lock momentarily - otherwise we'd
	//need to release the lock every time we bail!() or call a fallible operation, since the
	//error-handling code will try to immutably borrow `frames` when it builds a stack trace.
	frames: RefCell<Vec<Frame>>,

	recursion: Cell<u32>
}

pub(crate) struct Stacks {
	pub(crate) regs: Vec<Slot>,
	stays: Vec<Option<Gc<Stay>>>,
	defers: Vec<usize>
}

#[derive(Clone)]
pub(crate) enum Frame {

	//various `glsp::something` apis. the second field is the argument to glsp::load, 
	//glsp::parse_file, and similar apis.
	GlspApi(GlspApiName, Option<Filename>),

	//the `glsp::call` and `glsp::coro_run` apis.
	GlspCall(Option<Sym>),
	GlspCoroRun(Gc<Coro>),

	//an invocation of a macro-expander, for example nested within glsp::expand(), glsp::require(),
	//or (load). the arr is the form being expanded - inspecting the arr will reveal the callsite
	//(arr.span()) and the macro's name (arr.get(0)). when an expander override fn is passed to
	//glsp::expand_1() or (expand-1), the second field is its name.
	Expand(Gc<Arr>, Option<Option<Sym>>),

	//a vm Instr which recursively invokes call(): Call2, OpCallMet, etc. the first field is a 
	//copy of the callee, and the second field is the callsite.
	Call(Slot, Span),

	//an Instr, such as LoadGlobal or OpAdd. ike ErrorAt, these are only pushed to the stack if 
	//the code is about to fail. they fulfil the same role as the Call stack entry: providing a 
	//name and callsite for ops and instrs, which would otherwise be anonymous.
	Instr(InstrName, Span),
	OpInstr(Sym, Span),

	//an invocation of error_at!(span, ...), bail_at or ensure_at. this entry is pushed to the
	//framestack just before calling GError::new(). without this entry, errors would be much
	//less precise... just "somewhere within this call", rather than pinpointing an instruction.
	ErrorAt(Span)
}

//it's important for performance to keep Frame as small as possible, so we specify a set of
//known GlspApis and Instrs, rather than just storing a &'static str pointer.
#[derive(Copy, Clone)]
#[allow(dead_code)]
pub(crate) enum GlspApiName {
	Parse,
	ParseAll,
	ParseFile,
	Parse1,
	Eval,
	EvalMulti,
	Require,
	Load,
	LoadStr,
	LoadAndCompile,
	LoadAndCompileStr,
	LoadAndCompileVals,
	LoadCompiled,
	Expand,
	ExpandMulti,
	Expand1,
	CoroFinish
}

impl fmt::Display for GlspApiName {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use GlspApiName::*;

		let name = match *self {
			Parse => "parse",
			ParseAll => "parse_all",
			ParseFile => "parse_file",
			Parse1 => "parse_1",
			Eval => "eval",
			EvalMulti => "eval-multi",
			Require => "require",
			Load => "load",
			LoadStr => "load_str",
			LoadAndCompile => "load_and_compile",
			LoadAndCompileStr => "load_and_compile_str",
			LoadAndCompileVals => "load_and_compile_vals",
			LoadCompiled => "load_compiled",
			Expand => "expand",
			ExpandMulti => "expand-multi",
			Expand1 => "expand_1",
			CoroFinish => "coro_finish"
		};

		write!(f, "{}", name)
	}
}

#[derive(Copy, Clone)]
pub(crate) enum InstrName {
	LoadGlobal,
	SetGlobal
}

impl fmt::Display for InstrName {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use InstrName::*;

		let name = match *self {
			LoadGlobal => "a global variable access",
			SetGlobal => "a global variable mutation"
		};

		write!(f, "{}", name)
	}
}

impl Vm {
	pub(crate) fn new() -> Vm {
		Vm {
			stacks: RefCell::new(Stacks {
				regs: Vec::with_capacity(256),
				stays: Vec::with_capacity(32),
				defers: Vec::with_capacity(16)
			}),
			frames: RefCell::new(Vec::with_capacity(32)),
			recursion: Cell::new(0)
		}
	}

	pub(crate) fn push_frame(&self, frame: Frame) {
		self.frames.borrow_mut().push(frame)
	}

	pub(crate) fn pop_frame(&self) -> Frame {
		self.frames.borrow_mut().pop().unwrap()
	}

	pub(crate) fn in_expander(&self) -> bool {
		self.frames.borrow().iter().rev().any(|frame| matches!(frame, Frame::Expand(..)))
	}

	pub(crate) fn expander_cur_span(&self) -> Span {
		for frame in self.frames.borrow().iter().rev() {
			match frame {
				Frame::Call(_, span) => return *span,
				Frame::Expand(arr, _) => {
					let name = arr.get::<Sym>(0).ok();
					let storage = Expanded(name, arr.span(), Span::default());
					return glsp::span(storage)
				}
				_ => ()
			}
		}

		unreachable!()
	}

	pub(crate) fn traverse_stacks(&self) {
		with_heap(|heap| {
			let stacks = self.stacks.borrow();
			let frames = self.frames.borrow();

			for reg in &stacks.regs {
				heap.traverse_stack_slot(reg);
			}

			for stay in &stacks.stays {
				if let Some(ref stay) = *stay {
					heap.traverse_stack_gc(stay);
				}
			}

			for frame in &*frames {
				use Frame::*;

				match frame {
					GlspApi(_, _) => (),
					GlspCall(_) => (),
					GlspCoroRun(coro) => heap.traverse_stack_gc(coro),
					Expand(arr, _) => heap.traverse_stack_gc(arr),
					Call(slot, _) => heap.traverse_stack_slot(slot),
					Instr(_, _) => (),
					OpInstr(_, _) => (),
					ErrorAt(_) => ()
				}
			}
		});
	}

	pub(crate) fn exec_bytecode(&self, bytecode: &Root<Bytecode>) -> GResult<Val> {
		exec_bytecode(self, bytecode)
	}

	pub(crate) fn exec_gfn(&self, gfn: &Root<GFn>, arg_count: usize) -> GResult<Val> {
		exec_gfn(self, gfn, arg_count)
	}

	pub(crate) fn coro_run(&self, coro: &Root<Coro>, resume_arg: Option<Val>) -> GResult<Val> {
		coro_run(self, coro, resume_arg, false)
	}

	pub(crate) fn coro_finish(&self, coro: &Root<Coro>) -> GResult<()> {
		coro_run(self, coro, None, true)?;
		Ok(())
	}

	//the brief file location reported by (try) or (file-location), e.g. "scripts/main.glsp:10".
	//searches for the innermost Span in the callstack - this will be precise for individual
	//vm instructions and individual parsed forms, thanks to bail_at!(). if a file location can't
	//be identified, returns Ok(false) and writes nothing to `f`.
	pub(crate) fn file_location<F: fmt::Write>(&self, f: &mut F) -> Result<bool, fmt::Error> {
		use Frame::*;

		let frames = self.frames.borrow();
		for frame in frames.iter().rev() {
			let span = match frame {
				Call(_, span) | Instr(_, span) | OpInstr(_, span) | ErrorAt(span) => {
					*span
				}
				Expand(arr, _) => {
					match arr.span() {
						g if g == Span::default() => continue,
						span => span
					}
				}
				GlspApi(_, _) | GlspCall(_) | GlspCoroRun(_) => {
					continue
				}
			};

			if glsp::span_file_location(f, span)? {
				return Ok(true)
			}
		}

		Ok(false)
	}

	//the full stack trace reported by (try-verbose) or (stack-trace). a fairly straightforward
	//translation of the frame-stack into text, with no leading or trailing linebreaks.
	pub(crate) fn stack_trace<F: fmt::Write>(&self, f: &mut F) -> fmt::Result {
		use Frame::*;

		let frames = self.frames.borrow();
		let mut iter = frames.iter().peekable();
		while let Some(frame) = iter.next() {

			match frame {
				GlspApi(name, None) => {
					write!(f, "glsp::{}()", name)?;
				}
				GlspApi(name, Some(filename)) => {
					write!(f, "glsp::{}({:?})", name, glsp::filename_str(*filename))?;
				}
				GlspCall(name) => {
					match name {
						Some(name) => write!(f, "glsp::call(), invoking ({})", name)?,
						None => write!(f, "glsp::call(), invoking an anonymous function")?,
					}
				}
				GlspCoroRun(coro) => {
					match coro.storage.borrow().gfn.as_ref().unwrap().name() {
						Some(name) => write!(f, "glsp::coro_run(), running the fn ({})", name)?,
						None => write!(f, "glsp::coro_run(), running an anonymous fn")?
					}
				}
				Expand(arr, override_name) => {
					let write_name = |f: &mut F| {
						if arr.len() >= 1 && arr.get::<Val>(0).unwrap().is_sym() {
							write!(f, "the macro ({})", arr.get::<Sym>(0).unwrap())
						} else {
							write!(f, "an anonymous macro")
						}
					};

					let span = arr.span();
					glsp::span_context(f, span, &|f| {
						match override_name {
							None => {
								write!(f, "expanding ")?;
								write_name(f)
							}
							Some(None) => {
								write!(f, "expanding ")?;
								write_name(f)?;
								write!(f, " with an anonymous expander override")
							}
							Some(Some(override_name)) => {
								write!(f, "expanding ")?;
								write_name(f)?;
								write!(f, " using the expander ({})", override_name)
							}
						}
					})?;
				}
				Call(callee, span) => {
					glsp::span_context(f, *span, &|f| {
						let name = match callee {
							Slot::GFn(gfn) => gfn.name(),
							Slot::RFn(rfn) => rfn.name(),
							Slot::Class(class) => class.name(),
							_ => None
						};

						match name {
							Some(name) => write!(f, "({})", name),
							None => write!(f, "an anonymous fn")
						}
					})?;
				}
				Instr(message, span) => {
					glsp::span_context(f, *span, &|f| {
						write!(f, "{}", message)
					})?;
				}
				OpInstr(name, span) => {
					glsp::span_context(f, *span, &|f| {
						write!(f, "({})", name)
					})?;
				}
				ErrorAt(span) => {
					glsp::span_context(f, *span, &|f| {
						write!(f, "processing a form")
					})?;
				}
			}

			if iter.peek().is_some() {
				write!(f, "\n")?;
			}
		}

		Ok(())
	}

	pub(crate) fn clear(&self) {
		let mut stacks = self.stacks.borrow_mut();
		stacks.regs.clear();
		stacks.stays.clear();

		self.frames.borrow_mut().clear();
	}
}


//-------------------------------------------------------------------------------------------------
// entrypoints to the interpreter
//-------------------------------------------------------------------------------------------------

//execute a naked Bytecode. currently only invoked by glsp::load and glsp::eval.
fn exec_bytecode(
	vm: &Vm,
	bytecode: &Root<Bytecode>
) -> GResult<Val> {

	let instr_n = 0;

	let mut stacks = vm.stacks.borrow_mut();
	let base_reg = stacks.regs.len();
	let base_stay = stacks.stays.len();
	let base_defer = stacks.defers.len();

	//regs
	stacks.regs.extend_from_slice(&bytecode.start_regs[..]);

	//stays
	stacks.stays.extend(bytecode.start_stays.iter().map(|stay_source| {
		match stay_source {
			StaySource::Empty => None,
			StaySource::PreExisting(gc_stay) => Some(gc_stay.clone()),
			StaySource::Captured(_) => unreachable!(),
			StaySource::Param(_) => unreachable!()
		}
	}));

	//cleanup code
	let _guard = Guard::new(|| {
		let mut stacks = vm.stacks.borrow_mut();
		stacks.regs.truncate(base_reg);
		stacks.stays.truncate(base_stay);
	});

	//invoke the interpreter
	drop(stacks);
	match interpret(vm, bytecode.to_gc(), instr_n, base_reg, base_stay) {
		Ok(InterpretResult::Return(slot)) => Ok(slot.into_root()),
		Ok(InterpretResult::Yield(_, _, _)) => unreachable!(),
		Ok(InterpretResult::EndDefer) => unreachable!(),
		Err(mut error) => {
			run_defers(vm, bytecode.to_gc(), base_reg, base_stay, base_defer, &mut error);
			Err(error)
		}
	}
}

//execute a gfn, as though calling it. used by the public-facing glsp::call api, but *not* used by
//glsp-to-glsp calls (they invoke call() directly).
fn exec_gfn(
	vm: &Vm, 
	gfn: &Root<GFn>,
	arg_count: usize
) -> GResult<Val> {

	let result = call(
		vm, 
		vm.stacks.borrow_mut(), 
		0,
		Slot::GFn(gfn.to_gc()),
		arg_count,
		None
	)?;

	Ok(result.into_root())
}

//put a Coro's saved data back onto the stack and resume its execution until it yields, returns
//or fails. used by glsp::coro_run. if the `defers_only` flag is set, the coro is not resumed;
//instead, any pending `defers` are run and the coro's state is set to `finished`.
fn coro_run(
	vm: &Vm,
	coro: &Root<Coro>,
	resume_arg: Option<Val>,
	defers_only: bool
) -> GResult<Val> {

	//check the coro's current state
	let resume_reg = match coro.state.get() {
		PrivCoroState::Newborn => None,
		PrivCoroState::Paused(resume_reg) => Some(resume_reg),
		PrivCoroState::Running => bail!("coro is currently running"),
		PrivCoroState::Finished => bail!("coro has already finished"),
		PrivCoroState::Poisoned => bail!("coro has been poisoned by a previous error"),
		PrivCoroState::Recycling => unreachable!()
	};

	//drain the coro's regs, stays and defers, appending them to the vm's stacks
	let mut stacks = vm.stacks.borrow_mut();
	let base_reg = stacks.regs.len();
	let base_stay = stacks.stays.len();
	let base_defer = stacks.defers.len();

	let usage0 = coro.owned_memory_usage();
	let mut storage = coro.storage.borrow_mut();

	stacks.regs.extend(storage.regs.drain(..));
	stacks.stays.extend(storage.stays.drain(..));
	stacks.defers.extend(storage.defers.drain(..));

	let gfn = storage.gfn.clone().unwrap();
	let instr = storage.instr;

	drop(storage);
	let usage1 = coro.owned_memory_usage();

	with_heap(|heap| {
		heap.memory_usage_barrier(&**coro, usage0, usage1);
	});

	//set the coro's state to Running
	coro.state.set(PrivCoroState::Running);

	//use RAII for cleanup
	let cleanup_guard = Guard::new(|| {
		let mut stacks = vm.stacks.borrow_mut();
		stacks.regs.truncate(base_reg);
		stacks.stays.truncate(base_stay);
	});

	//move `stacks` here so that it's dropped before `cleanup_guard`
	let mut stacks = stacks;

	//assign the resume_arg to the resume_reg
	match (resume_arg, resume_reg) {
		(Some(arg), Some(reg)) => stacks.regs[base_reg + reg as usize] = Slot::from_val(&arg),
		(None, Some(reg)) => stacks.regs[base_reg + reg as usize] = Slot::Nil,
		(Some(_), None) => bail!("the first invocation of coro-run cannot receive an argument"),
		(None, None) => ()
	}

	//if `defers_only` is set, execute any pending (defer)s, set the state to `finished` or
	//`poisoned`, then return.
	if defers_only {
		let mut error: Option<GError> = None;

		while stacks.defers.len() > base_defer {
			let defer_instr = stacks.defers.pop().unwrap();

			drop(stacks);
			match interpret(vm, gfn.lambda.bytecode.clone(), defer_instr, base_reg, base_stay) {
				Ok(InterpretResult::EndDefer) => (),
				Ok(InterpretResult::Return(..)) => unreachable!(),
				Ok(InterpretResult::Yield(..)) => unreachable!(),
				Err(new_error) => {
					if error.is_some() {
						error.as_mut().unwrap().chain_defer_error(new_error);
					} else {
						error = Some(new_error);
					}
				}
			}
			stacks = vm.stacks.borrow_mut();
		}

		if let Some(error) = error {
			coro.state.set(PrivCoroState::Poisoned);
			return Err(error)
		} else {
			coro.state.set(PrivCoroState::Finished);
			return Ok(Val::Nil)
		}
	}

	//run the interpreter, then update the coro's state to either Paused or Finished. in the event 
	//of an error, run any pending (defer)s and set the coro's state to Poisoned.
	drop(stacks);
	match interpret(vm, gfn.lambda.bytecode.clone(), instr, base_reg, base_stay) {
		Ok(InterpretResult::Return(slot)) => {
			coro.state.set(PrivCoroState::Finished);
			Ok(slot.into_root())
		}
		Ok(InterpretResult::Yield(slot, resume_reg, resume_instr)) => {
			coro.state.set(PrivCoroState::Paused(resume_reg));

			let mut storage = coro.storage.borrow_mut();
			storage.instr = resume_instr;

			let mut stacks = vm.stacks.borrow_mut();
			storage.regs.extend(stacks.regs.drain(base_reg..));
			storage.stays.extend(stacks.stays.drain(base_stay..));
			storage.defers.extend(stacks.defers.drain(base_defer..));
			drop(stacks);

			drop(storage);
			forget(cleanup_guard);

			let usage2 = coro.owned_memory_usage();

			coro.write_barrier();
			with_heap(|heap| {
				heap.memory_usage_barrier(&**coro, usage1, usage2);
			});
			
			Ok(slot.into_root())
		}
		Ok(InterpretResult::EndDefer) => unreachable!(),
		Err(mut error) => {
			coro.state.set(PrivCoroState::Poisoned);
			run_defers(vm, gfn.lambda.bytecode.clone(), base_reg, base_stay, 
			           base_defer, &mut error);
			Err(error)
		}
	}
}

//it feels wrong to mark such a large fn as #[inline(always)], but it saves about 11ns per call
#[inline(always)]
fn call<'a>(
	vm: &'a Vm,
	mut stacks: RefMut<'a, Stacks>,
	mut splay_bits: u32,
	callee: Slot,
	mut arg_count: usize,
	callsite: Option<Span>
) -> GResult<Slot> {
	
	//splay arguments in-place
	if splay_bits != 0 {
		let mut index = stacks.regs.len() - arg_count;
		let base_index = index;

		while splay_bits != 0 {
			if splay_bits & 0x1 == 0x1 {
				let giter = match stacks.regs[index].clone() {
					Slot::Arr(arr) => {
						stacks.regs.splice(index..index + 1, 
						                    arr.iter_to::<Slot>().map(|s| s.unwrap()));
						index += arr.len();
						splay_bits >>= 1;
						continue
					}
					Slot::Str(st) => {
						stacks.regs.splice(index..index + 1, st.iter().map(Slot::Char));
						index += st.len();
						splay_bits >>= 1;
						continue
					}
					Slot::Tab(tab) => tab.giter(),
					Slot::Coro(coro) => coro.giter(),
					Slot::GIter(giter) => giter.root(),
					slot => bail!("attempted to splay {}", slot.a_type_name())
				};

				//advancing a GIter may invoke arbitrary GameLisp code, so we can't emit our
				//results directly to the reg stack
				drop(stacks);
				let mut vec = SmallVec::<[Slot; 8]>::new();

				if let GIterLen::Exact(exact_len) = giter.len() {
					vec.reserve(exact_len);
				}

				for result in giter {
					vec.push(Slot::from_val(&result?));
				}

				stacks = vm.stacks.borrow_mut();

				let vec_len = vec.len();
				stacks.regs.splice(index..index + 1, vec);
				index += vec_len;
			} else {
				index += 1;
			}

			splay_bits >>= 1;
		}

		arg_count = stacks.regs.len() - base_index;
	}

	//if the argument is an rfn or class, call it and return. if it's a yielding gfn, create and 
	//return a new Coro. if it's a non-yielding gfn, recursively invoke the interpreter.
	match callee {
		Slot::RFn(rfn) => {
			drop(stacks);

			glsp::call_rfn(rfn, arg_count)
		}
		Slot::Class(class) => {
			drop(stacks);

			Ok(Slot::Obj(Gc::from_root(&glsp::call_class(&class.root(), arg_count)?)))
		}
		Slot::GFn(gfn) if gfn.yields() => {
			let mut regs = Vec::with_capacity(gfn.lambda.bytecode.start_regs.len());
			let mut stays = Vec::with_capacity(gfn.lambda.bytecode.start_stays.len());

			let base_reg = stacks.regs.len() - arg_count;
			regs.extend(stacks.regs.drain(base_reg..));

			wrangle_args_and_stays(
				&mut regs,
				&mut stays,
				arg_count,
				&gfn,
				callsite
			)?;

			let coro = glsp::alloc_gc(Coro::new(gfn, regs, stays));
			coro.write_barrier();

			Ok(Slot::Coro(coro))
		}
		Slot::GFn(gfn) => {
			let base_reg = stacks.regs.len() - arg_count;
			let base_stay = stacks.stays.len();
			let base_defer = stacks.defers.len();

			let _guard = Guard::new(|| {
				let mut stacks = vm.stacks.borrow_mut();
				stacks.regs.truncate(base_reg);
				stacks.stays.truncate(base_stay);
			});

			//moving `stacks` here so that it will be dropped before `_guard` if wrangling fails
			let mut stacks = stacks;

			//convert the args on the stack into the regs and stays for a new instance of the
			//interpreter
			let Stacks { ref mut regs, ref mut stays, .. } = *stacks;
			wrangle_args_and_stays(
				regs,
				stays,
				arg_count,
				&gfn,
				callsite
			)?;
			drop(regs);
			drop(stays);

			//recurse into the interpreter. if an error bubbles through, run any pending (defer)s.
			drop(stacks);
			match interpret(vm, gfn.lambda.bytecode.clone(), 0, base_reg, base_stay) {
				Ok(InterpretResult::Return(slot)) => Ok(slot),
				Ok(InterpretResult::Yield(_, _, _)) => unreachable!(),
				Ok(InterpretResult::EndDefer) => unreachable!(),
				Err(mut error) => {
					run_defers(vm, gfn.lambda.bytecode.clone(), base_reg, base_stay, 
					           base_defer, &mut error);
					Err(error)
				}
			}
		}
		ref val => bail!("attempted to call {}", val.a_type_name())
	}
}

//the last `arg_count` elements of `regs` must be the arguments to a gfn call (after splaying). 
//replaces those arguments with an initial set of registers for the given gfn, pushes the initial 
//stays for that gfn onto `stays`, and returns the first instr which should be executed in the 
//gfn's bytecode.
#[inline]
fn wrangle_args_and_stays(
	regs: &mut Vec<Slot>,
	stays: &mut Vec<Option<Gc<Stay>>>,
	arg_count: usize,
	gfn: &GFn,
	callsite: Option<Span>
) -> GResult<()> {

	let param_map = &gfn.lambda.param_map;
	let bytecode = &gfn.lambda.bytecode;
	let base_reg = regs.len() - arg_count;

	//the raw arguments have already been pushed to the reg stack, before this function was called.
	//wrangle them into one register for each parameter.
	match param_map.wrangle_args(regs, &bytecode.start_regs[..], arg_count, callsite) {
		Ok(_) => (),
		Err(msg) => {
			match gfn.lambda.name {
				Some(name) => bail!("unable to call the fn '{}': {}", name, msg),
				None => bail!("{}", msg)
			}
		}
	}

	//push the remaining local, scratch and literal registers onto the reg stack
	regs.extend((&bytecode.start_regs[param_map.param_count..]).iter().cloned());

	//allocate this frame's stays, if any, and push them onto the stay stack
	if bytecode.start_stays.len() > 0 {
		stays.reserve(bytecode.start_stays.len());

		for source in &bytecode.start_stays {
			let stay = match *source {
				StaySource::Empty => None,
				StaySource::Param(i) => {
					Some(glsp::alloc_gc(Stay::new(regs[base_reg + i as usize].clone())))
				}
				StaySource::Captured(capture_id) => {
					Some(gfn.captured_stays[capture_id as usize].clone())
				}
				StaySource::PreExisting(ref pre_existing) => {
					Some(pre_existing.clone())
				}
			};

			stays.push(stay);
		}
	}

	Ok(())
}

//run any pending defers, after a call to interpret() returns Err(_)
fn run_defers(
	vm: &Vm,
	bytecode: Gc<Bytecode>,
	base_reg: usize,
	base_stay: usize,
	base_defer: usize,
	error: &mut GError
) {
	let mut stacks = vm.stacks.borrow_mut();
	while stacks.defers.len() > base_defer {
		let defer_instr = stacks.defers.pop().unwrap();

		drop(stacks);
		match interpret(vm, bytecode.clone(), defer_instr, base_reg, base_stay) {
			Ok(InterpretResult::EndDefer) => (),
			Ok(InterpretResult::Return(_)) | Ok(InterpretResult::Yield(..)) => unreachable!(),
			Err(defer_error) => {
				error.chain_defer_error(defer_error);
			}
		}
		stacks = vm.stacks.borrow_mut();
	}
}


//-------------------------------------------------------------------------------------------------
// the interpreter
//-------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub(crate) enum InterpretResult {
	Return(Slot),
	Yield(Slot, u8, usize), //(result, resume_reg, resume_instr)
	EndDefer
}

//we've chosen quite a conservative recursion limit, because the native stack limit on win32 is
//1mb, and rustc currently compiles interpret() in a memory-hungry way: 1kb to 2kb per gfn call.
const RECURSION_LIMIT: u32 = 256;

fn interpret(
	vm: &Vm,
	bytecode: Gc<Bytecode>,
	mut instr_n: usize,
	base_reg: usize,
	base_stay: usize
) -> GResult<InterpretResult> {

	//check the recursion limit
	let recursion = vm.recursion.get();
	if recursion >= RECURSION_LIMIT {
		bail!("recursion limit exceeded (more than {} nested interpreters)", RECURSION_LIMIT);
	}
	vm.recursion.set(recursion + 1);
	let _recursion_guard = Guard::new(|| vm.recursion.set(recursion));

	//interpreter state
	let mut stacks = vm.stacks.borrow_mut();
	let mut splay_bits = 0u32;

	//for readability, we deliberately don't indent the outermost loop
	loop {

	//we read the instruction to interpret, then immediately increment instr_n so that it's 
	//referring to the next instruction that will be interpreted after this one (if any). 
	//this makes instructions like Jump a little more intuitive.
	let instr = bytecode.instrs[instr_n];
	let cur_span = bytecode.spans[instr_n];
	instr_n += 1;

	//macros
	macro_rules! reg(
		($i:expr) => (stacks.regs[base_reg + $i as usize]);
	);

	macro_rules! stay(
		($i:expr) => (stacks.stays[base_stay + $i as usize]);
	);

	macro_rules! bail_instr(
		($instr_name:expr, $($arg:tt)+) => ({
			vm.frames.borrow_mut().push(Frame::Instr($instr_name, cur_span));
			let _guard = Guard::new(|| { vm.frames.borrow_mut().pop().unwrap(); });

			bail!($($arg)+)
		});
	);

	macro_rules! bail_op(
		($op_sym:expr, $($arg:tt)+) => ({
			vm.frames.borrow_mut().push(Frame::OpInstr($op_sym, cur_span));
			let _guard = Guard::new(|| { vm.frames.borrow_mut().pop().unwrap(); });

			bail!($($arg)+)
		});
	);

	macro_rules! numeric_op(
		($op_sym:expr, $dst_reg:expr, $arg0_reg:expr, $arg1_reg:expr, 
		 $int_op:expr, $flo_op:expr) => ({
			let int_op = $int_op;
			let flo_op = $flo_op;
			let result = match (&reg!($arg0_reg), &reg!($arg1_reg)) {
				(&Slot::Int(i0), &Slot::Int(i1)) => Slot::Int(int_op(i0, i1)),
				(&Slot::Int(i), &Slot::Flo(f)) => Slot::Flo(flo_op(i as f32, f)),
				(&Slot::Flo(f), &Slot::Int(i)) => Slot::Flo(flo_op(f, i as f32)),
				(&Slot::Flo(f0), &Slot::Flo(f1)) => Slot::Flo(flo_op(f0, f1)),
				_ => bail_op!($op_sym, "non-number passed to a numeric op")
			};
	
			reg!($dst_reg) = result;
		});
	);

	macro_rules! numeric_cmp_op(
		($op_sym:expr, $dst_reg:expr, $arg0_reg:expr, $arg1_reg:expr, 
		 $int_op:expr, $flo_op:expr) => ({
			let int_op = $int_op;
			let flo_op = $flo_op;

			let arg0 = match &reg!($arg0_reg) {
				&Slot::Int(i0) => Slot::Int(i0),
				&Slot::Flo(f0) => Slot::Flo(f0),
				&Slot::Char(c0) => Slot::Int(c0 as u32 as i32),
				_ => bail_op!($op_sym, "non-number passed to a numeric comparison op")
			};

			let arg1 = match &reg!($arg1_reg) {
				&Slot::Int(i1) => Slot::Int(i1),
				&Slot::Flo(f1) => Slot::Flo(f1),
				&Slot::Char(c1) => Slot::Int(c1 as u32 as i32),
				_ => bail_op!($op_sym, "non-number passed to a numeric comparison op")
			};

			let result = match (arg0, arg1) {
				(Slot::Int(i0), Slot::Int(i1)) => Slot::Bool(int_op(i0, i1)),
				(Slot::Int(i), Slot::Flo(f)) => Slot::Bool(flo_op(i as f32, f)),
				(Slot::Flo(f), Slot::Int(i)) => Slot::Bool(flo_op(f, i as f32)),
				(Slot::Flo(f0), Slot::Flo(f1)) => Slot::Bool(flo_op(f0, f1)),
				_ => unreachable!()
			};
			
			reg!($dst_reg) = result;
		});
	);

	//interpret the instruction
	match instr {
		Instr::CopyRegister(dst_reg, src_reg) => {
			reg!(dst_reg) = reg!(src_reg).clone();
		}
		Instr::LoadGlobal(dst_reg, sym_bytes) => {
			let sym = Sym::from(sym_bytes);

			if let Some(global) = glsp::try_global(sym).unwrap() {
				reg!(dst_reg) = Slot::from_val(&global);
			} else {
				bail_instr!(InstrName::LoadGlobal, "unbound symbol '{}'", sym)
			}
		}
		Instr::SetGlobal(src_reg, sym_bytes) => {
			use glsp::TrySetGlobalOutcome::*;

			let sym = Sym::from(sym_bytes);
			
			match glsp::try_set_global(sym, &reg!(src_reg)).unwrap() {
				Success => (),
				NotBound => bail_instr!(InstrName::SetGlobal, "unbound symbol '{}'", sym),
				Frozen => bail_instr!(InstrName::SetGlobal, "global '{}' is frozen", sym)
			}
		}
		Instr::LoadStay(dst_reg, stay_id) => {
			reg!(dst_reg) = stay!(stay_id).clone().unwrap().get();
		}
		Instr::SetStay(src_reg, stay_id) => {
			stay!(stay_id).clone().unwrap().set(reg!(src_reg).clone());
		}
		Instr::MakeStay(src_reg, stay_id) => {
			stay!(stay_id) = Some(glsp::alloc_gc(Stay::new(reg!(src_reg).clone())));
		}
		Instr::MakeGFn(dst_reg, lambda_id) => {
			let lambda = &bytecode.lambdas[lambda_id as usize];
			let captures = Vec::from_iter(lambda.captures.iter().map(|&stay_id| {
				stay!(stay_id).clone().unwrap()
			}));

			reg!(dst_reg) = Slot::GFn(glsp::alloc_gc(GFn::new(lambda, captures)));
		}
		Instr::Call0(dst_reg, callee_reg) => {
			let callee = reg!(callee_reg).clone();

			vm.frames.borrow_mut().push(Frame::Call(callee.clone(), cur_span));
			let _guard = Guard::new(|| {
				vm.frames.borrow_mut().pop().unwrap();
			});

			let result = call(
				vm,
				stacks, 
				replace(&mut splay_bits, 0), 
				callee, 
				0,
				Some(cur_span)
			)?;

			stacks = vm.stacks.borrow_mut();
			reg!(dst_reg) = result;
		}
		Instr::Call1(dst_reg, callee_reg, arg_reg) => {
			let arg = reg!(arg_reg).clone();
			stacks.regs.push(arg);

			let callee = reg!(callee_reg).clone();

			vm.frames.borrow_mut().push(Frame::Call(callee.clone(), cur_span));
			let _guard = Guard::new(|| {
				vm.frames.borrow_mut().pop().unwrap();
			});

			let result = call(
				vm,
				stacks, 
				replace(&mut splay_bits, 0), 
				callee, 
				1,
				Some(cur_span)
			)?;

			stacks = vm.stacks.borrow_mut();
			reg!(dst_reg) = result;
		}
		Instr::Call2(dst_reg, callee_reg, arg0_reg, arg1_reg) => {
			let arg0 = reg!(arg0_reg).clone();
			let arg1 = reg!(arg1_reg).clone();
			stacks.regs.push(arg0);
			stacks.regs.push(arg1);

			let callee = reg!(callee_reg).clone();

			vm.frames.borrow_mut().push(Frame::Call(callee.clone(), cur_span));
			let _guard = Guard::new(|| {
				vm.frames.borrow_mut().pop().unwrap();
			});

			let result = call(
				vm,
				stacks, 
				replace(&mut splay_bits, 0), 
				callee, 
				2,
				Some(cur_span)
			)?;

			stacks = vm.stacks.borrow_mut();
			reg!(dst_reg) = result;
		}
		Instr::CallN(dst_reg, base_reg, arg_count) => {
			for arg_reg in base_reg + 1 .. base_reg + 1 + arg_count {
				let arg = reg!(arg_reg).clone();
				stacks.regs.push(arg);
			}

			let callee = reg!(base_reg).clone();

			vm.frames.borrow_mut().push(Frame::Call(callee.clone(), cur_span));
			let _guard = Guard::new(|| {
				vm.frames.borrow_mut().pop().unwrap();
			});

			let result = call(
				vm,
				stacks, 
				replace(&mut splay_bits, 0), 
				callee, 
				arg_count as usize,
				Some(cur_span)
			)?;

			stacks = vm.stacks.borrow_mut();
			reg!(dst_reg) = result;
		}
		Instr::Splay(splay_bytes) => {
			splay_bits = u32::from_ne_bytes(splay_bytes);

			debug_assert!(
				matches!(
					bytecode.instrs[instr_n],
					Instr::Call1(..) | Instr::Call2(..) | Instr::CallN(..) | Instr::OpArr(..)
				)
			);
		}
		Instr::Return(src_reg) => {
			return Ok(InterpretResult::Return(reg!(src_reg).clone()))
		}
		Instr::Yield(dst_reg, src_reg) => {
			return Ok(InterpretResult::Yield(reg!(src_reg).clone(), dst_reg, instr_n))
		}
		Instr::Jump(jump_bytes) => {
			instr_n = ((instr_n as isize) + isize::from(jump_bytes)) as usize;
		}
		Instr::JumpIfTrue(src_reg, jump_bytes) => {
			let reg_true = match reg!(src_reg) {
				Slot::Nil => false,
				Slot::Bool(b) => b,
				_ => true
			};
			
			if reg_true {
				instr_n = ((instr_n as isize) + isize::from(jump_bytes)) as usize;
			}
		}
		Instr::JumpIfFalse(src_reg, jump_bytes) => {
			let reg_true = match reg!(src_reg) {
				Slot::Nil => false,
				Slot::Bool(b) => b,
				_ => true
			};
			
			if !reg_true {
				instr_n = ((instr_n as isize) + isize::from(jump_bytes)) as usize;
			}
		}
		Instr::PushDefer(defer_id) => {
			let defer_instr = bytecode.defers[defer_id as usize];
			stacks.defers.push(defer_instr);
		}
		Instr::RunAndPopDefers(defer_count) => {
			let mut error: Option<GError> = None;

			for _ in 0 .. defer_count {
				let defer_instr = stacks.defers.pop().unwrap();
				drop(stacks);

				match interpret(vm, bytecode.clone(), defer_instr, base_reg, base_stay) {
					Ok(InterpretResult::EndDefer) => (),
					Ok(InterpretResult::Return(..)) => unreachable!(),
					Ok(InterpretResult::Yield(..)) => unreachable!(),
					Err(new_error) => {
						if error.is_some() {
							error.as_mut().unwrap().chain_defer_error(new_error);
						} else {
							error = Some(new_error);
						}
					}
				}

				stacks = vm.stacks.borrow_mut();
			}

			if let Some(err) = error {
				 return Err(err)
			}
		}
		Instr::RunDefer(defer_id) => {
			let defer_instr = bytecode.defers[defer_id as usize];

			drop(stacks);
			match interpret(vm, bytecode.clone(), defer_instr, base_reg, base_stay)? {
				InterpretResult::EndDefer => (),
				InterpretResult::Return(..) | InterpretResult::Yield(..) => unreachable!()
			}
			stacks = vm.stacks.borrow_mut();
		}
		Instr::EndDefer() => {
			return Ok(InterpretResult::EndDefer)
		}
		Instr::OpAdd(dst_reg, arg0_reg, arg1_reg) => {
			numeric_op!(
				ADD_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0: i32, i1: i32| i0.wrapping_add(i1), 
				|f0, f1| f0 + f1
			)
		}
		Instr::OpSub(dst_reg, arg0_reg, arg1_reg) => {
			numeric_op!(
				SUB_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0: i32, i1: i32| i0.wrapping_sub(i1), 
				|f0, f1| f0 - f1
			)
		}
		Instr::OpMul(dst_reg, arg0_reg, arg1_reg) => {
			numeric_op!(
				MUL_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0: i32, i1: i32| i0.wrapping_mul(i1), 
				|f0, f1| f0 * f1
			)
		}
		Instr::OpDiv(dst_reg, arg0_reg, arg1_reg) => {
			//we can't implement this using numeric_op because the int version is fallible
			match (&reg!(arg0_reg), &reg!(arg1_reg)) {
				(&Slot::Int(i0), &Slot::Int(i1)) => {
					if i1 == 0 {
						bail_op!(DIV_SYM, "divide-by-zero error")
					}
					reg!(dst_reg) = Slot::Int(i0.wrapping_div(i1));
				}
				(&Slot::Int(i), &Slot::Flo(f)) => {
					reg!(dst_reg) = Slot::Flo((i as f32)/f);
				}
				(&Slot::Flo(f), &Slot::Int(i)) => {
					reg!(dst_reg) = Slot::Flo(f/(i as f32));
				}
				(&Slot::Flo(f0), &Slot::Flo(f1)) => {
					reg!(dst_reg) = Slot::Flo(f0/f1);
				}
				_ => bail_op!(DIV_SYM, "non-number passed to /")
			}
		}
		Instr::OpAbs(dst_reg, arg_reg) => {
			match reg!(arg_reg) {
				Slot::Int(i) => reg!(dst_reg) = Slot::Int(i.wrapping_abs()),
				Slot::Flo(f) => reg!(dst_reg) = Slot::Flo(f.abs()),
				_ => bail_op!(ABS_SYM, "non-number passed to the abs function")
			}
		}
		Instr::OpNeg(dst_reg, arg_reg) => {
			match reg!(arg_reg) {
				Slot::Int(i) => reg!(dst_reg) = Slot::Int(i.wrapping_neg()),
				Slot::Flo(f) => reg!(dst_reg) = Slot::Flo(-f),
				_ => bail_op!(SUB_SYM, "non-number passed to the - function")
			}
		}
		Instr::OpRem(dst_reg, arg0_reg, arg1_reg) => {
			//like OpDiv, we can't implement this using numeric_op
			match (&reg!(arg0_reg), &reg!(arg1_reg)) {
				(&Slot::Int(i0), &Slot::Int(i1)) => {
					if i1 == 0 {
						bail_op!(REM_SYM, "divide-by-zero error")
					}
					reg!(dst_reg) = Slot::Int(i0.wrapping_rem(i1));
				}
				(&Slot::Int(i), &Slot::Flo(f)) => {
					reg!(dst_reg) = Slot::Flo((i as f32)%f);
				}
				(&Slot::Flo(f), &Slot::Int(i)) => {
					reg!(dst_reg) = Slot::Flo(f%(i as f32));
				}
				(&Slot::Flo(f0), &Slot::Flo(f1)) => {
					reg!(dst_reg) = Slot::Flo(f0%f1);
				}
				_ => bail_op!(REM_SYM, "non-number passed to %")
			}
		}
		Instr::OpSign(dst_reg, arg_reg) => {
			match reg!(arg_reg) {
				Slot::Int(i) => reg!(dst_reg) = Slot::Int(i.signum()),
				Slot::Flo(f) => {
					let sign = if f == 0.0f32 { 
						0
					} else if f.is_nan() {
						0
					} else {
						f.signum() as i32
					};
					reg!(dst_reg) = Slot::Int(sign);
				}
				_ => bail_op!(SIGN_SYM, "non-number passed to the sign function")
			}
		}
		Instr::OpMin(dst_reg, arg0_reg, arg1_reg) => {
			let slot0 = &reg!(arg0_reg);
			let slot1 = &reg!(arg1_reg);

			if !(matches!(slot0, Slot::Int(_) | Slot::Flo(_) | Slot::Char(_)) &&
			     matches!(slot1, Slot::Int(_) | Slot::Flo(_) | Slot::Char(_))) {
				bail_op!(MIN_SYM, "invalid argument passed to min")
			}

			//todo: the rooting here isn't ideal
			match slot0.root().partial_cmp(&slot1.root()) {
				Some(Ordering::Less) | Some(Ordering::Equal) => reg!(dst_reg) = slot0.clone(),
				Some(Ordering::Greater) => reg!(dst_reg) = slot1.clone(),
				None => unreachable!()
			}
		}
		Instr::OpMax(dst_reg, arg0_reg, arg1_reg) => {
			let slot0 = &reg!(arg0_reg);
			let slot1 = &reg!(arg1_reg);

			if !(matches!(slot0, Slot::Int(_) | Slot::Flo(_) | Slot::Char(_)) &&
			     matches!(slot1, Slot::Int(_) | Slot::Flo(_) | Slot::Char(_))) {
				bail_op!(MAX_SYM, "invalid argument passed to max")
			}

			//todo: the rooting here isn't ideal
			match slot0.root().partial_cmp(&slot1.root()) {
				Some(Ordering::Greater) | Some(Ordering::Equal) => reg!(dst_reg) = slot0.clone(),
				Some(Ordering::Less) => reg!(dst_reg) = slot1.clone(),
				None => unreachable!()
			}
		}
		Instr::OpPredicate(dst_reg, arg_reg, predicate) => {
			let result = match (predicate, &reg!(arg_reg)) {
				(Predicate::Nil, &Slot::Nil) => true,
				(Predicate::Num, &Slot::Int(_)) => true,
				(Predicate::Num, &Slot::Flo(_)) => true,
				(Predicate::Int, &Slot::Int(_)) => true,
				(Predicate::Flo, &Slot::Flo(_)) => true,
				(Predicate::Nan, &Slot::Flo(f)) => f.is_nan(),
				(Predicate::Inf, &Slot::Flo(f)) => f.is_infinite(),
				(Predicate::Bool, &Slot::Bool(_)) => true,
				(Predicate::Sym, &Slot::Sym(_)) => true,
				(Predicate::Deque, &Slot::Arr(_)) => true,
				(Predicate::Deque, &Slot::Str(_)) => true,
				(Predicate::Arr, &Slot::Arr(_)) => true,
				(Predicate::Str, &Slot::Str(_)) => true,
				(Predicate::Tab, &Slot::Tab(_)) => true,
				(Predicate::GIter, &Slot::GIter(_)) => true,
				(Predicate::Iterable, &Slot::Arr(_)) => true,
				(Predicate::Iterable, &Slot::Str(_)) => true,
				(Predicate::Iterable, &Slot::Tab(_)) => true,
				(Predicate::Iterable, &Slot::GIter(_)) => true,
				(Predicate::Iterable, &Slot::Coro(_)) => true,
				(Predicate::Obj, &Slot::Obj(_)) => true,
				(Predicate::Class, &Slot::Class(_)) => true,
				(Predicate::Callable, &Slot::GFn(_)) => true,
				(Predicate::Callable, &Slot::RFn(_)) => true,
				(Predicate::Callable, &Slot::Class(_)) => true,
				(Predicate::Expander, &Slot::GFn(_)) => true,
				(Predicate::Expander, &Slot::RFn(_)) => true,
				(Predicate::GFn, &Slot::GFn(_)) => true,
				(Predicate::RFn, &Slot::RFn(_)) => true,
				(Predicate::Coro, &Slot::Coro(_)) => true,
				(Predicate::RData, &Slot::RData(_)) => true,
				_ => false
			};

			reg!(dst_reg) = Slot::Bool(result);
		}
		Instr::OpInt(dst_reg, arg_reg) => {
			match reg!(arg_reg) {
				Slot::Int(i) => reg!(dst_reg) = Slot::Int(i),
				Slot::Flo(f) => reg!(dst_reg) = Slot::Int(f as i32),
				Slot::Char(c) => reg!(dst_reg) = Slot::Int(c as i32),
				Slot::Bool(b) => reg!(dst_reg) = Slot::Int(b as i32),
				_ => bail_op!(INT_SYM, "cannot cast argument to an int")
			}
		}
		Instr::OpFlo(dst_reg, arg_reg) => {
			match reg!(arg_reg) {
				Slot::Int(i) => reg!(dst_reg) = Slot::Flo(i as f32),
				Slot::Flo(f) => reg!(dst_reg) = Slot::Flo(f),
				Slot::Char(c) => reg!(dst_reg) = Slot::Flo(c as i32 as f32),
				Slot::Bool(b) => reg!(dst_reg) = Slot::Flo(b as i32 as f32),
				_ => bail_op!(FLO_SYM, "cannot cast argument to a flo")
			}
		}
		Instr::OpBool(dst_reg, arg_reg) => {
			let result = match reg!(arg_reg) {
				Slot::Bool(b) => b,
				Slot::Nil => false,
				_ => true
			};

			reg!(dst_reg) = Slot::Bool(result);
		}
		Instr::OpNumEq(dst_reg, arg0_reg, arg1_reg) => {
			numeric_cmp_op!(
				NUM_EQ_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0, i1| i0 == i1, 
				|f0, f1| float_total_eq(f0, f1)
			)
		}
		Instr::OpLt(dst_reg, arg0_reg, arg1_reg) => {
			numeric_cmp_op!(
				LT_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0, i1| i0 < i1, 
				|f0, f1| float_total_cmp(f0, f1) == Ordering::Less
			)
		}
		Instr::OpLte(dst_reg, arg0_reg, arg1_reg) => {
			numeric_cmp_op!(
				LTE_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0, i1| i0 <= i1, 
				|f0, f1| float_total_cmp(f0, f1) != Ordering::Greater
			)
		}
		Instr::OpGt(dst_reg, arg0_reg, arg1_reg) => {
			numeric_cmp_op!(
				GT_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0, i1| i0 > i1, 
				|f0, f1| float_total_cmp(f0, f1) == Ordering::Greater
			)
		}
		Instr::OpGte(dst_reg, arg0_reg, arg1_reg) => {
			numeric_cmp_op!(
				GTE_SYM, 
				dst_reg, arg0_reg, arg1_reg, 
				|i0, i1| i0 >= i1, 
				|f0, f1| float_total_cmp(f0, f1) != Ordering::Less
			)
		}
		Instr::OpNot(dst_reg, arg_reg) => {
			let reg_true = match reg!(arg_reg) {
				Slot::Bool(b) => b,
				Slot::Nil => false,
				_ => true
			};
			
			reg!(dst_reg) = Slot::Bool(!reg_true);
		}
		Instr::OpIter(dst_reg, arg_reg) => {
			let giter = match &reg!(arg_reg) {
				Slot::Arr(arr) => arr.giter().to_gc(),
				Slot::Str(st) => st.giter().to_gc(),
				Slot::Tab(tab) => tab.giter().to_gc(),
				Slot::Coro(coro) => coro.giter().to_gc(),
				Slot::GIter(giter) => giter.clone(),
				slot => bail_op!(ITER_SYM, "attempted to iterate {}", slot.a_type_name())
			};
			
			reg!(dst_reg) = Slot::GIter(giter);
		}
		Instr::OpIterNext(dst_reg, arg_reg) => {
			let giter = match &reg!(arg_reg) {
				Slot::GIter(giter) => giter.clone(),
				slot => bail_op!(ITER_NEXT_SYM, "expected an iter, received {}", 
				                 slot.a_type_name())
			};

			//raw_next() can recurse into arbitrary code, so we need to update the frame-stack and
			//drop our borrowed reg stacks
			vm.frames.borrow_mut().push(Frame::OpInstr(ITER_NEXT_SYM, cur_span));
			let _guard = Guard::new(|| {
				vm.frames.borrow_mut().pop().unwrap();
			});

			drop(stacks);
			let result = giter.raw_next();
			stacks = vm.stacks.borrow_mut();

			match result {
				None => reg!(dst_reg) = Slot::Nil,
				Some(Ok(slot)) => reg!(dst_reg) = slot,
				Some(Err(err)) => return Err(err)
			}
		}
		Instr::OpIterNextBack(dst_reg, arg_reg) => {
			let giter = match &reg!(arg_reg) {
				Slot::GIter(giter) => giter.clone(),
				slot => bail_op!(ITER_NEXT_BACK_SYM, "expected an iter, received {}", 
				                 slot.a_type_name())
			};

			vm.frames.borrow_mut().push(Frame::OpInstr(ITER_NEXT_BACK_SYM, cur_span));
			let _guard = Guard::new(|| {
				vm.frames.borrow_mut().pop().unwrap();
			});

			drop(stacks);
			let result = giter.raw_next_back();
			stacks = vm.stacks.borrow_mut();

			match result {
				None => reg!(dst_reg) = Slot::Nil,
				Some(Ok(slot)) => reg!(dst_reg) = slot,
				Some(Err(err)) => return Err(err)
			}
		}
		Instr::OpIterFinishedp(dst_reg, arg_reg) => {
			match &reg!(arg_reg) {
				Slot::GIter(giter) => {
					reg!(dst_reg) = Slot::Bool(giter.is_finished());
				}
				slot => bail_op!(ITER_FINISHEDP_SYM, "expected an iter, received {}", 
				                 slot.a_type_name())
			};
		}
		Instr::OpLen(dst_reg, arg_reg) => {
			match reg!(arg_reg) {
				Slot::Arr(ref arr) => reg!(dst_reg) = Slot::Int(arr.len() as i32),
				Slot::Str(ref st) => reg!(dst_reg) = Slot::Int(st.len() as i32),
				Slot::Tab(ref tab) => reg!(dst_reg) = Slot::Int(tab.len() as i32),
				Slot::GIter(ref giter) => {
					match giter.len() {
						GIterLen::Exact(len) => reg!(dst_reg) = Slot::Int(len as i32),
						GIterLen::Infinite => reg!(dst_reg) = Slot::Sym(INFINITE_SYM),
						GIterLen::Unknown => reg!(dst_reg) = Slot::Sym(UNKNOWN_SYM)
					}
				}
				_ => bail_op!(LEN_SYM, "non-arr/str/tab/iter passed to the len builtin")
			}
		}
		Instr::OpHasp(dst_reg, arg0_reg, arg1_reg) => {
			let table: Slot = reg!(arg0_reg).clone();
			let key: Slot = reg!(arg1_reg).clone();

			let result = match table {
				Slot::Arr(ref arr) => {
					if let Slot::Int(i) = key {
						let len = arr.len() as i32;
						i >= -len && i < len
					} else {
						false
					}
				}
				Slot::Tab(ref tab) => {
					tab.has(key).unwrap()
				}
				Slot::Obj(ref obj) => {
					if let Slot::Sym(key_name) = key {
						obj.has(key_name).unwrap()
					} else {
						false
					}
				}
				Slot::Class(ref class) => {
					if let Slot::Sym(key_name) = key {
						class.has(key_name).unwrap()
					} else {
						false
					}
				}
				ref slot => {
					bail_op!(HASP_SYM, "expected an arr, tab, obj or class, but received {}", 
					         slot.a_type_name())
				}
			};

			reg!(dst_reg) = Slot::Bool(result);
		}
		Instr::OpAccess(dst_reg, arg0_reg, arg1_reg) => {
			let coll = reg!(arg0_reg).clone();
			let index = reg!(arg1_reg).clone();

			match coll {
				Slot::Arr(_) | Slot::Str(_) => {
					if let Slot::Int(raw_index) = index {
						//we can't allow the arr.get() or st.get() implementation to produce a 
						//GError here, so we have to do our own bounds-checking.
						let len = match coll {
							Slot::Arr(ref arr) => arr.len(),
							Slot::Str(ref st) => st.len(),
							_ => unreachable!()
						};

						let index = if raw_index < 0 {
							(len as isize) + (raw_index as isize)
						} else {
							raw_index as isize
						};

						if index < 0 || (index as usize) >= len  {
							bail_op!(ACCESS_SYM, "out-of-bounds {} access: len is {}, index is {}", 
							         coll.a_type_name(), len, raw_index)
						}
						
						match coll {
							Slot::Arr(ref arr) => {
								reg!(dst_reg) = arr.get::<Slot>(index as usize).unwrap();
							}
							Slot::Str(ref st) => {
								reg!(dst_reg) = Slot::Char(st.get::<char>(index).unwrap());
							}
							_ => unreachable!()
						}
					} else {
						let src_giter = match index {
							Slot::Arr(arr) => arr.giter(),
							Slot::Str(st) => st.giter(),
							Slot::Tab(tab) => tab.giter(),
							Slot::Coro(coro) => coro.giter(),
							Slot::GIter(giter) => giter.root(),
							index => bail_op!(ACCESS_SYM, "indexed {} with {}",
							                  coll.a_type_name(), index.a_type_name())
						};

						let giter = match coll {
							Slot::Arr(ref arr) => Arr::access_giter(&arr.root(), &src_giter),
							Slot::Str(ref st) => Str::access_giter(&st.root(), &src_giter),
							_ => unreachable!()
						};

						reg!(dst_reg) = Slot::GIter(giter.into_gc());
					}
				}
				Slot::Tab(ref tab) => {
					if let Ok(Some(value)) = tab.get_if_present::<_, Slot>(&index) {
						reg!(dst_reg) = value;
					} else {
						bail_op!(ACCESS_SYM, "key {:?} is not present", index)
					}
				}
				Slot::Obj(ref obj) => {
					if let Slot::Sym(key_name) = index {
						if obj.is_killed() {
							bail_op!(ACCESS_SYM, "attempted to access a killed obj")
						}

						//because get_if_present could invoke a property getter, it can fail. 
						//(todo: add a Frame variant for a property getter)
						drop(stacks);
						if let Some(value) = obj.get_if_present::<_, Slot>(key_name)? {
							stacks = vm.stacks.borrow_mut();
							reg!(dst_reg) = value;
						} else {
							bail_op!(ACCESS_SYM, "key {:?} is not present", index)
						}
					} else {
						let src_giter = match index {
							Slot::Arr(arr) => arr.giter(),
							Slot::Str(st) => st.giter(),
							Slot::Tab(tab) => tab.giter(),
							Slot::Coro(coro) => coro.giter(),
							Slot::GIter(giter) => giter.root(),
							slot => bail_op!(ACCESS_SYM, "indexed an obj with {}",
							                 slot.a_type_name())
						};

						let giter = Obj::access_giter(&obj.root(), &src_giter);
						reg!(dst_reg) = Slot::GIter(giter.into_gc());
					}
				}
				Slot::RData(ref rdata) => {
					if let Slot::Sym(key_name) = index {
						//because get_if_present could invoke a property getter, it can fail. 
						//(todo: add a Frame variant for a property getter)
						drop(stacks);
						if let Some(value) = rdata.get_if_present::<_, Slot>(key_name)? {
							stacks = vm.stacks.borrow_mut();
							reg!(dst_reg) = value;
						} else {
							bail_op!(ACCESS_SYM, "key {:?} is not present", index)
						}
					} else {
						let src_giter = match index {
							Slot::Arr(arr) => arr.giter(),
							Slot::Str(st) => st.giter(),
							Slot::Tab(tab) => tab.giter(),
							Slot::Coro(coro) => coro.giter(),
							Slot::GIter(giter) => giter.root(),
							slot => bail_op!(ACCESS_SYM, "indexed an rdata with {}",
							                 slot.a_type_name())
						};

						let giter = RData::access_giter(&rdata.root(), &src_giter);
						reg!(dst_reg) = Slot::GIter(giter.into_gc());
					}
				}
				Slot::Class(ref class) => {
					if let Slot::Sym(key_name) = index {
						if let Ok(Some(value)) = class.get_if_present::<_, Slot>(key_name) {
							reg!(dst_reg) = value;
						} else {
							bail_op!(ACCESS_SYM, "key {:?} is not present", key_name)
						}
					} else {
						let src_giter = match index {
							Slot::Arr(arr) => arr.giter(),
							Slot::Str(st) => st.giter(),
							Slot::Tab(tab) => tab.giter(),
							Slot::Coro(coro) => coro.giter(),
							Slot::GIter(giter) => giter.root(),
							slot => bail_op!(ACCESS_SYM, "indexed a class with {}",
							                 slot.a_type_name())
						};

						let giter = Class::access_giter(&class.root(), &src_giter);
						reg!(dst_reg) = Slot::GIter(giter.into_gc());
					}	
				}
				slot => bail_op!(ACCESS_SYM, "attempted to index {}", slot.a_type_name())
			}		
		}
		Instr::OpSetAccess(dst_reg, arg0_reg, arg1_reg, arg2_reg) => {
			let coll = reg!(arg0_reg).clone();
			let index = reg!(arg1_reg).clone();
			let new_value = reg!(arg2_reg).clone();

			match coll {
				Slot::Arr(_) | Slot::Str(_) => {
					let raw_index = match index {
						Slot::Int(i) => i,
						_ => bail_op!(SET_ACCESS_SYM, "{} indexed with a non-int", 
						              coll.a_type_name())
					};

					//we can't allow the arr.set() or st.set() implementation to produce a 
					//GError here, so we have to do our own bounds-checks and can-mutate-checks.
					let len = match reg!(arg0_reg) {
						Slot::Arr(ref arr) => arr.len(),
						Slot::Str(ref st) => st.len(),
						_ => unreachable!()
					};

					let index = if raw_index < 0 {
						(len as isize) + (raw_index as isize)
					} else {
						raw_index as isize
					};

					if index < 0 || (index as usize) >= len  {
						bail_op!(SET_ACCESS_SYM, "out-of-bounds {} access: len is {}, index is {}", 
						         coll.type_name(), len, raw_index)
					}
					
					match reg!(arg0_reg) {
						Slot::Arr(ref arr) => {
							if !arr.can_mutate() {
								bail_op!(SET_ACCESS_SYM, "attempted to mutate a frozen arr")
							}

							arr.set(index, &reg!(arg2_reg)).unwrap();
						}
						Slot::Str(ref st) => {
							if !st.can_mutate() {
								bail_op!(SET_ACCESS_SYM, "attempted to mutate a frozen str")
							}

							let ch = match reg!(arg2_reg) {
								Slot::Char(ch) => ch,
								_ => bail_op!(SET_ACCESS_SYM, "non-char assigned to a str")
							};
							
							st.set(index, &ch).unwrap();
						}
						_ => unreachable!()
					}
				}
				Slot::Tab(ref tab) => {
					if !tab.can_mutate() {
						bail_op!(SET_ACCESS_SYM, "attempted to mutate a frozen tab")
					}
					
					tab.set(index, new_value).unwrap();
				}
				Slot::Obj(ref obj) => {
					if obj.is_frozen() {
						bail_op!(SET_ACCESS_SYM, "attempted to mutate a frozen obj")
					}

					if obj.is_killed() {
						bail_op!(SET_ACCESS_SYM, "attempted to access a killed obj")
					}

					match index {
						Slot::Sym(key_name) => {
							//because set_if_present could invoke a property setter, it can fail. 
							//(todo: add a Frame variant for a property setter)
							drop(stacks);
							if !obj.set_if_present(key_name, new_value)? {
								bail_op!(SET_ACCESS_SYM, "attempted to mutate nonexistent \
								         field '{}'", key_name)
							}
							stacks = vm.stacks.borrow_mut();
						}
						_ => {
							bail_op!(SET_ACCESS_SYM, "attempted to mutate non-sym obj field")
						}
					}
				}
				Slot::RData(ref rdata) => {
					match index {
						Slot::Sym(key_name) => {
							//because set_if_present could invoke a property setter, it can fail. 
							//(todo: add a Frame variant for a property setter)
							drop(stacks);
							if !rdata.set_if_present(key_name, new_value)? {
								bail_op!(SET_ACCESS_SYM, "attempted to mutate nonexistent \
								         rdata prop '{}'", key_name)
							}
							stacks = vm.stacks.borrow_mut();
						}
						_ => bail_op!(SET_ACCESS_SYM, "attempted to mutate non-sym rdata prop")
					}
				}
				slot => bail_op!(SET_ACCESS_SYM, "attempted to index {}", slot.a_type_name())
			}	

			reg!(dst_reg) = Slot::Nil;	
		}
		Instr::OpArr(dst_reg, arg0_reg, arg_count) => {
			let arr = if splay_bits != 0 {
				let mut bits = replace(&mut splay_bits, 0);

				//we iterate over the input twice to make sure we have an exact capacity...
				//it's a little expensive, but a reallocation would be even more expensive
				let mut capacity = 0;
				let mut tmp_bits = bits;
				for i in 0 .. arg_count {
					if tmp_bits & 0x1 == 0x1 {
						match reg!(arg0_reg + i) {
							Slot::Arr(ref src) => capacity += src.len(),
							Slot::Str(ref src) => capacity += src.len(),
							Slot::Tab(ref src) => capacity += src.len(),
							Slot::Coro(_) => {
								capacity = 0;
								break
							}
							Slot::GIter(ref src) => {
								match src.len() {
									GIterLen::Exact(exact_len) => capacity += exact_len,
									_ => {
										capacity = 0;
										break
									}
								}
							}
							ref slot => bail_op!(ARR_SYM, "splayed {}", slot.a_type_name())
						}
					} else {
						capacity += 1;
					}

					tmp_bits >>= 1;
				}

				let arr = with_heap(|heap| heap.recycler.arr_with_capacity(capacity));
				for i in 0 .. arg_count {
					if bits & 0x1 == 0x1 {
						let giter = match reg!(arg0_reg + i) {
							Slot::Arr(ref src) => {
								arr.extend(src.iter_to::<Slot>().map(|s| s.unwrap())).unwrap();
								None
							}
							Slot::Str(ref src) => {
								arr.extend(src.iter().map(Slot::Char)).unwrap();
								None
							}
							Slot::Tab(ref src) => Some(src.giter()),
							Slot::Coro(ref src) => Some(src.giter()),
							Slot::GIter(ref src) => Some(src.root()),
							_ => unreachable!()
						};

						if let Some(giter) = giter {
							drop(stacks);

							if let GIterLen::Exact(exact_len) = giter.len() {
								arr.reserve(exact_len).unwrap();
							}

							vm.frames.borrow_mut().push(Frame::OpInstr(ARR_SYM, cur_span));
							let _guard = Guard::new(|| {
								vm.frames.borrow_mut().pop().unwrap();
							});

							for result in giter {
								arr.push(result?).unwrap();
							}

							stacks = vm.stacks.borrow_mut();
						}
					} else {
						arr.push(reg!(arg0_reg + i).clone()).unwrap();
					}

					bits >>= 1;
				}

				arr
			} else {
				with_heap(|heap| {
					heap.recycler.arr_from_iter((0 .. arg_count).map(|i| {
						reg!(arg0_reg + i).clone()
					})).unwrap()
				})
			};

			arr.set_span(glsp::new_arr_span(Some(cur_span)));
			reg!(dst_reg) = Slot::Arr(arr.into_gc());
		}
		Instr::OpCallMet(dst_reg, arg0_reg, arg_count) => {
			if arg_count < 2 {
				bail_op!(CALL_MET_SYM, "expected 2 or more args, but received {}", arg_count)
			}

			for i in arg0_reg .. arg0_reg + arg_count {
				let arg = (reg!(i)).clone();
				stacks.regs.push(arg);
			}
			let base_index = stacks.regs.len() - arg_count as usize;

			//the first two arguments are a receiver obj, and a method name. use them to look
			//up the actual gfn or rfn which is to be called, and some flags indicating whether
			//it expects `self` and `next_index` arguments.
			let method_name = match stacks.regs[base_index] {
				Slot::Sym(method_name) => method_name,
				_ => bail_op!(CALL_MET_SYM, "the first argument to call-met must be a sym")
			};

			let receiver = stacks.regs[base_index + 1].clone();
			let tuple = match receiver {
				Slot::Obj(ref obj) => {
					//because get_method could invoke a property getter, it can fail. (todo:
					//add a Frame variant for a property getter)
					drop(stacks);
					let tuple = obj.get_method(method_name)?;
					stacks = vm.stacks.borrow_mut();
					tuple
				}
				Slot::RData(ref rdata) => rdata.get_method(method_name),
				Slot::Class(ref class) => class.get_method(method_name),
				_ => bail_op!(CALL_MET_SYM, "the second argument to call-met must be an \
				              obj or an rdata")
			};

			let (callee, expects_self, expects_ni, ni) = match tuple {
				Some(tuple) => tuple,
				None => bail_op!(CALL_MET_SYM, "attempted to call nonexistent method '{}'", 
						         method_name)
			};

			vm.frames.borrow_mut().push(Frame::Call(callee.clone(), cur_span));
			let _guard = Guard::new(|| {
				vm.frames.borrow_mut().pop().unwrap();
			});

			//shuffle around the arguments on the reg stack, then delegate to call()
			let mut first_arg_index = if expects_ni {
				stacks.regs[base_index] = receiver;
				stacks.regs[base_index + 1] = ni;
				base_index
			} else {
				base_index + 1
			};

			if !expects_self {
				first_arg_index += 1;
			}

			let call_arg_count = stacks.regs.len() - first_arg_index;
			let result = call(
				vm,
				stacks, 
				0, 
				callee, 
				call_arg_count,
				Some(cur_span)
			)?;

			stacks = vm.stacks.borrow_mut();
			stacks.regs.truncate(base_index);
			reg!(dst_reg) = result;
		}
		Instr::OpCallMetOpt(dst_reg, arg0_reg, arg_count) => {
			if arg_count < 2 {
				bail_op!(CALL_MET_OPT_SYM, "expected 2 or more args, but received {}", arg_count)
			}

			for i in arg0_reg .. arg0_reg + arg_count {
				let arg = (reg!(i)).clone();
				stacks.regs.push(arg);
			}
			let base_index = stacks.regs.len() - arg_count as usize;

			let method_name = match stacks.regs[base_index] {
				Slot::Sym(method_name) => method_name,
				_ => bail_op!(CALL_MET_OPT_SYM, 
				              "the first argument to call-met-opt must be a sym")
			};

			let receiver = stacks.regs[base_index + 1].clone();
			let tuple = match receiver {
				Slot::Obj(ref obj) => {
					//because get_method could invoke a property getter, it can fail. (todo:
					//add a Frame variant for a property getter)
					drop(stacks);
					let tuple = obj.get_method(method_name)?;
					stacks = vm.stacks.borrow_mut();
					tuple
				}
				Slot::RData(ref rdata) => rdata.get_method(method_name),
				Slot::Class(ref class) => class.get_method(method_name),
				_ => bail_op!(CALL_MET_OPT_SYM, "the second argument to call-met-opt must \
				              be an obj or an rdata")
			};

			if let Some(tuple) = tuple {
				let (callee, expects_self, expects_ni, ni) = tuple;

				vm.frames.borrow_mut().push(Frame::Call(callee.clone(), cur_span));
				let _guard = Guard::new(|| {
					vm.frames.borrow_mut().pop().unwrap();
				});

				let mut first_arg_index = if expects_ni {
					stacks.regs[base_index] = receiver;
					stacks.regs[base_index + 1] = ni;
					base_index
				} else {
					base_index + 1
				};

				if !expects_self {
					first_arg_index += 1;
				}

				let call_arg_count = stacks.regs.len() - first_arg_index;
				let result = call(
					vm,
					stacks, 
					0, 
					callee, 
					call_arg_count,
					Some(cur_span)
				)?;

				stacks = vm.stacks.borrow_mut();
				stacks.regs.truncate(base_index);
				reg!(dst_reg) = result;
			} else {
				reg!(dst_reg) = Slot::Nil;
				stacks.regs.truncate(base_index);
			}
		}
		Instr::OpCallBaseRaw(dst_reg, arg0_reg, arg_count) => {
			if arg_count < 2 {
				bail_op!(CALL_BASE_RAW_SYM, "expected 2 or more args, but received {}", arg_count)
			}

			for i in arg0_reg .. arg0_reg + arg_count {
				let arg = (reg!(i)).clone();
				stacks.regs.push(arg);
			}
			let base_index = stacks.regs.len() - arg_count as usize;

			let obj = match stacks.regs[base_index] {
				Slot::Obj(ref obj) => obj.clone(),
				_ => panic!()
			};

			match stacks.regs[base_index + 1] {
				Slot::Nil => {
					reg!(dst_reg) = Slot::Nil;
					stacks.regs.truncate(base_index);
				}
				Slot::Int(raw_index) if raw_index >= 0 => {
					if let Some(met_lookup) = obj.get_base_raw_method(raw_index as usize) {
						if met_lookup.requires_next_index {
							stacks.regs[base_index + 1] = match met_lookup.next_index {
								Some(index) => Slot::Int(index as i32),
								None => Slot::Nil
							};
						} else {
							stacks.regs.remove(base_index + 1);
						}

						let callee = Slot::GFn(met_lookup.gfn.clone());
						vm.frames.borrow_mut().push(Frame::Call(callee.clone(), cur_span));
						let _guard = Guard::new(|| {
							vm.frames.borrow_mut().pop().unwrap();
						});

						let call_arg_count = stacks.regs.len() - base_index;
						let result = call(
							vm,
							stacks, 
							0, 
							callee, 
							call_arg_count,
							Some(cur_span)
						)?;

						stacks = vm.stacks.borrow_mut();
						reg!(dst_reg) = result;
					} else {
						reg!(dst_reg) = Slot::Nil;
						stacks.regs.truncate(base_index);
					}
				}
				_ => panic!()
			}
		}
		Instr::OpGlobal(dst_reg, arg_reg) => {
			let sym = match reg!(arg_reg) {
				Slot::Sym(sym) => sym,
				ref slot => bail_op!(GLOBAL_SYM, "expected a sym, received {}", 
				                     slot.a_type_name())
			};

			reg!(dst_reg) = match glsp::try_global(sym).unwrap() {
				Some(val) => Slot::from_val(&val),
				None => bail_op!(GLOBAL_SYM, "unbound symbol '{}'", sym)
			}
		}
		Instr::OpSetGlobal(dst_reg, arg0_reg, arg1_reg) => {
			use glsp::TrySetGlobalOutcome::*;

			let sym = match reg!(arg0_reg) {
				Slot::Sym(sym) => sym,
				ref slot => bail_op!(SET_GLOBAL_SYM, "expected a sym, received {}", 
				                     slot.a_type_name())
			};

			match glsp::try_set_global(sym, &reg!(arg1_reg)).unwrap() {
				Success => reg!(dst_reg) = Slot::Nil,
				NotBound => bail_op!(SET_GLOBAL_SYM, "unbound symbol '{}'", sym),
				Frozen => bail_op!(SET_GLOBAL_SYM, "global '{}' is frozen", sym)
			}
		}
	}

	} //end of loop
}
