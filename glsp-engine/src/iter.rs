use std::{usize};
use std::cell::{RefCell};
use std::cmp::{min};
use super::class::{Class, Obj};
use super::code::{Coro, CoroState, GFn};
use super::collections::{Arr, DequeAccess, DequeOps, Str, Tab};
use super::engine::{glsp, RData, RFn, with_heap};
use super::error::{GResult};
use super::gc::{Allocate, Gc, GcHeader, Root, Slot, Visitor};
use super::val::{Val};
use super::wrap::{Callable, FromVal};

//-------------------------------------------------------------------------------------------------
// GIter
//-------------------------------------------------------------------------------------------------

/**
The `iter` primitive type.

The name `GIter` was chosen to avoid confusion with Rust's iterators.

GameLisp iterators are always stored on the garbage-collected heap, so they're normally 
represented by the type [`Root<GIter>`](struct.Root.html).

It's possible to use a `Root<GIter>` as a Rust iterator. However, collection types like 
[`Arr`](struct.Arr.html) also provide methods to create native Rust iterators, which will 
generally have much better performance.
*/

pub struct GIter {
	header: GcHeader,
	pub(crate) state: RefCell<GIterState>
}

impl Allocate for GIter {
	fn header(&self) -> &GcHeader {
		&self.header
	}

	fn clear_gcs(&self) {
		*self.state.borrow_mut() = GIterState::Finished;
	}

	fn visit_gcs<V: Visitor>(&self, v: &mut V) {
		use GIterState::*;

		match &*self.state.borrow() {
			Finished | Empty => (),
			RnExclusive(..) | RnInclusive(..) | RnOpen(..) => (),
			FRnExclusive(..) | FRnInclusive(..) | FRnOpen(..) => (),
			ArrElements(arr, ..) => v.visit_gc(arr),
			StrElements(st, ..) => v.visit_gc(st),
			TabEntries(arr) => v.visit_gc(arr),
			TabKeys(arr) => v.visit_gc(arr),
			TabValues(arr) => v.visit_gc(arr),
			CoroResults(coro) => v.visit_gc(coro),
			Once1(slot) => v.visit_slot(slot),
			OnceN(arr) => v.visit_gc(arr),
			OnceWith(callable) => visit_gc_callable(v, callable),
			Repeat1(slot) => v.visit_slot(slot),
			RepeatN(arr, ..) => v.visit_gc(arr),
			RepeatWith(callable) => visit_gc_callable(v, callable),
			AccessArr(arr, iter) => {
				v.visit_gc(arr);
				v.visit_gc(iter);
			}
			AccessStr(st, iter) => {
				v.visit_gc(st);
				v.visit_gc(iter);
			}
			AccessObj(obj, iter) => {
				v.visit_gc(obj);
				v.visit_gc(iter);
			}
			AccessRData(rdata, iter) => {
				v.visit_gc(rdata);
				v.visit_gc(iter);
			}
			AccessClass(class, iter) => {
				v.visit_gc(class);
				v.visit_gc(iter);
			},
			Chunks(_, arr) => v.visit_gc(arr),
			RChunks(_, arr) => v.visit_gc(arr),
			Windows(_, arr) => v.visit_gc(arr),
			Lines(st) => v.visit_gc(st),
			Split(src, split_at) => {
				v.visit_gc(src);
				v.visit_gc(split_at);
			}
			Rev(base) => v.visit_gc(base),
			Enumerate(base, _) => v.visit_gc(base),
			Cloned(base) => v.visit_gc(base),
			DeepCloned(base) => v.visit_gc(base),
			StepBy(_, base) => v.visit_gc(base),
			Map(callable, base) => {
				visit_gc_callable(v, callable);
				v.visit_gc(base);
			}
			Filter(callable, base) => {
				visit_gc_callable(v, callable);
				v.visit_gc(base);
			}
			Zip(arr) => v.visit_gc(arr),
			Chain(arr) => v.visit_gc(arr),
			Flatten(base, cur_giter) => {
				v.visit_gc(base);
				if let Some(cur_giter) = cur_giter {
					v.visit_gc(cur_giter);
				}
			}
			Cycle(base, collection, _) => {
				if let Some(base) = base {
					v.visit_gc(base);
				}
				v.visit_gc(collection);
			}
			Take(_, base) => v.visit_gc(base),
			TakeWhile(callable, base) => {
				visit_gc_callable(v, callable);
				v.visit_gc(base);
			}
			Skip(_, base) => v.visit_gc(base),
			SkipWhile(callable, base) => {
				if let Some(callable) = callable {
					visit_gc_callable(v, callable);
				}
				v.visit_gc(base);
			}
		}
	}

	fn owned_memory_usage(&self) -> usize {
		0
	}
}

impl Iterator for Root<GIter> {
	type Item = GResult<Val>;

	fn next(&mut self) -> Option<GResult<Val>> {
		(**self).next()
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		match (**self).len() {
			GIterLen::Exact(len) => (len, Some(len)),
			GIterLen::Infinite => (usize::MAX, None),
			GIterLen::Unknown => (0, None)
		}
	}
}

impl DoubleEndedIterator for Root<GIter> {
	fn next_back(&mut self) -> Option<GResult<Val>> {
		(**self).next_back()
	}
}

impl GIter {
	pub(crate) fn new(state: GIterState) -> GIter {
		GIter {
			header: GcHeader::new(),
			state: RefCell::new(state)
		}
	}

	/**
	Creates a shallow copy of the iterator.

	Equivalent to [`(clone it)`](https://gamelisp.rs/std/clone).
	*/
	pub fn shallow_clone(&self) -> Root<GIter> {
		glsp::giter((*self.state.borrow()).shallow_clone())
	}

	/**
	Returns `true` if the iterator has finished.

	Equivalent to [`(iter-finished? it)`](https://gamelisp.rs/std/iter-finished-p).
	*/
	pub fn is_finished(&self) -> bool {
		matches!(*self.state.borrow(), GIterState::Finished)
	}

	/**
	Returns the iterator's remaining length.

	Equivalent to [`(len it)`](https://gamelisp.rs/std/len).
	*/
	pub fn len(&self) -> GIterLen {
		use GIterState::*;
		use GIterLen::*;

		match *self.state.borrow() {
			Finished | Empty => Exact(0),
			RnExclusive(start, end, step_by) => {
				if step_by > 0 {
					if start < end {
						Exact(((end - start + (step_by - 1)) / step_by) as usize) 
					} else {
						Exact(0)
					}
				} else {
					if start > end {
						Exact(((start - end + (-step_by - 1)) / -step_by) as usize) 
					} else {
						Exact(0)
					}
				}
			}
			RnInclusive(start, end, step_by) => {
				if step_by > 0 {
					if start <= end {
						Exact(((end + 1 - start + (step_by - 1)) / step_by) as usize)
					} else {
						Exact(0)
					}
				} else {
					if start >= end {
						Exact(((start + 1 - end + (-step_by - 1)) / -step_by) as usize)
					} else {
						Exact(0)
					}
				}
			}
			RnOpen(..) => Infinite,
			FRnExclusive(..) | FRnInclusive(..) => Unknown, //due to float rounding error
			FRnOpen(..) => Infinite,
			ArrElements(ref _arr, _start_offs, _back_offs) => {
				//Exact((arr.len() as u32).saturating_sub(start_offs + back_offs) as usize)
				Unknown
			}
			StrElements(ref _st, _start_offs, _back_offs) => {
				//Exact((st.len() as u32).saturating_sub(start_offs + back_offs) as usize)
				Unknown
			}
			TabEntries(ref remaining) => Exact(remaining.len()),
			TabKeys(ref remaining) => Exact(remaining.len()),
			TabValues(ref remaining) => Exact(remaining.len()),
			CoroResults(_) => Unknown,
			Once1(_) => Exact(1),
			OnceN(ref arr) => Exact(arr.len()),
			OnceWith(_) => Exact(1),
			Repeat1(_) => Infinite,
			RepeatN(..) => Infinite,
			RepeatWith(_) => Infinite,
			AccessArr(_, ref giter) => giter.len(),
			AccessStr(_, ref giter) => giter.len(),
			AccessObj(_, ref giter) => giter.len(),
			AccessRData(_, ref giter) => giter.len(),
			AccessClass(_, ref giter) => giter.len(),
			Chunks(chunk_len, ref arr) | RChunks(chunk_len, ref arr) => {
				let clen = chunk_len as usize;
				Exact((arr.len() + (clen - 1)) / clen)
			}
			Windows(window_len, ref arr) => {
				Exact(arr.len().saturating_sub(window_len as usize - 1))
			}
			Lines(_) => Unknown,
			Split(_, _) => Unknown,
			Rev(ref base) => base.len(),
			Enumerate(ref base, _) => base.len(),
			Cloned(ref base) => base.len(),
			DeepCloned(ref base) => base.len(),
			StepBy(step_by, ref base) => { 
				match base.len() {
					Exact(len) => Exact(((len as u32 + step_by - 1) / step_by) as usize),
					Infinite => Infinite,
					Unknown => Unknown
				}
			}
			Map(_, ref base) => base.len(),
			Filter(_, ref base) => base.len(),
			Zip(ref arr) => {
				let mut min_len = Infinite;
				for val in arr.iter() {
					let giter = val.unwrap_giter();
					match giter.len() {
						Exact(len) => {
							match min_len {
								Exact(m) => min_len = Exact(min(m, len)),
								Infinite => min_len = Exact(len),
								Unknown => unreachable!()
							}
						}
						Unknown => return Unknown,
						Infinite => ()
					}
				}
				min_len
			}
			Chain(ref arr) => {
				let mut accum = 0;
				for val in arr.iter() {
					let giter = val.unwrap_giter();
					match giter.len() {
						Exact(len) => accum += len,
						Unknown => return Unknown,
						Infinite => return Infinite
					}
				}
				Exact(accum)
			}
			Flatten(_, _) => Unknown,
			Cycle(ref base, ref collection, _) => {
				match base {
					None => {
						if collection.len() == 0 {
							Exact(0)
						} else {
							Infinite
						}
					}
					Some(base) => {
						match base.len() {
							Exact(0) => Exact(0),
							Unknown => Unknown,
							_ => Infinite
						}
					}
				}
			}
			Take(remaining, ref base) => {
				match base.len() {
					Exact(n) if n <= remaining as usize => Exact(n),
					Exact(_) | Infinite => Exact(remaining as usize),
					Unknown => Unknown
				}
			}
			TakeWhile(_, _) => Unknown,
			Skip(remaining, ref base) => {
				match base.len() {
					Exact(n) => Exact(n.saturating_sub(remaining as usize)),
					Infinite => Infinite,
					Unknown => Unknown
				}
			}
			SkipWhile(_, _) => Unknown
		}
	}

	/**
	Returns `true` if the iterator is double-ended.

	Equivalent to [`(iter-double-ended? it)`](https://gamelisp.rs/std/iter-double-ended-p).
	*/
	pub fn is_double_ended(&self) -> bool {
		use GIterState::*;

		match *self.state.borrow() {
			Finished | Empty => true,
			RnExclusive(..) => true,
			RnInclusive(..) => true,
			RnOpen(..) => false,
			FRnExclusive(..) => false,
			FRnInclusive(..) => false,
			FRnOpen(..) => false,
			ArrElements(..) => true,
			StrElements(..) => true,
			TabEntries(..) => false,
			TabKeys(..) => false,
			TabValues(..) => false,
			CoroResults(..) => false,
			Once1(_) => true,
			OnceN(_) => true,
			OnceWith(_) => true,
			Repeat1(_) => true,
			RepeatN(..) => true,
			RepeatWith(_) => false,
			AccessArr(_, ref giter) => giter.is_double_ended(),
			AccessStr(_, ref giter) => giter.is_double_ended(),
			AccessObj(_, ref giter) => giter.is_double_ended(),
			AccessRData(_, ref giter) => giter.is_double_ended(),
			AccessClass(_, ref giter) => giter.is_double_ended(),
			Chunks(_, _) => true,
			RChunks(_, _) => true,
			Windows(_, _) => true,
			Lines(_) => true,
			Split(_, _) => true,
			Rev(_) => true,
			Enumerate(_, _) => false,
			Cloned(ref base) => base.is_double_ended(),
			DeepCloned(ref base) => base.is_double_ended(),
			StepBy(_, _) => false,
			Map(_, ref base) => base.is_double_ended(),
			Filter(_, ref base) => base.is_double_ended(),
			Zip(_) => false,
			Chain(ref arr) => arr.iter().all(|val| val.unwrap_giter().is_double_ended()),
			Flatten(_, _) => false,
			Cycle(_, _, _) => false,
			Take(_, _) => false,
			TakeWhile(_, _) => false,
			Skip(_, _) => false,
			SkipWhile(_, _) => false
		}
	}

	/**
	Advances the iterator and returns its next item.

	Equivalent to [`(iter-next! it)`](https://gamelisp.rs/std/iter-next-mut).
	*/
	pub fn next(&self) -> Option<GResult<Val>> {
		match self.raw_next() {
			Some(Ok(slot)) => Some(Ok(slot.root())),
			Some(Err(err)) => Some(Err(err)),
			None => None
		}
	}

	/**
	Advances the iterator from the back and returns its next item.

	Equivalent to [`(iter-next-back! it)`](https://gamelisp.rs/std/iter-next-back-mut).
	*/
	pub fn next_back(&self) -> Option<GResult<Val>> {
		match self.raw_next_back() {
			Some(Ok(slot)) => Some(Ok(slot.root())),
			Some(Err(err)) => Some(Err(err)),
			None => None
		}
	}

	fn write_barrier<T: Allocate>(&self, dst: &Gc<T>) {
		with_heap(|heap| heap.write_barrier(self, dst));
	}

	#[allow(dead_code)]
	fn write_barrier_slot(&self, slot: &Slot) {
		with_heap(|heap| heap.write_barrier_slot(self, slot));
	}

	//we don't want to do lots of rooting and unrooting when splaying call arguments in vm.rs, 
	//so we produce Slots rather than Vals. obviously this means that calling back to Rust or
	//GameLisp should be done with great care.
	pub(crate) fn raw_next(&self) -> Option<GResult<Slot>> {
		use GIterState::*;

		let mut state_ref = self.state.borrow_mut();
		let result = match *state_ref {
			Finished | Empty => None,
			RnExclusive(ref mut start, end, step_by) => {
				if (step_by > 0 && *start < end) || (step_by < 0 && *start > end) {
					let result = *start;
					*start += step_by;
					Some(Ok(Slot::Int(result)))
				} else {
					None
				}
			}
			RnInclusive(ref mut start, end, step_by) => {
				if (step_by > 0 && *start <= end) || (step_by < 0 && *start >= end) {
					let result = *start;
					*start += step_by;
					Some(Ok(Slot::Int(result)))
				} else {
					None
				}
			}
			RnOpen(ref mut start, step_by) => {
				let result = *start;
				*start += step_by;
				Some(Ok(Slot::Int(result)))
			}
			FRnExclusive(ref mut start, end, step_by) => {
				if (step_by > 0.0 && *start < end) || (step_by < 0.0 && *start > end) {
					let result = *start;
					*start += step_by;
					Some(Ok(Slot::Flo(result)))
				} else {
					None
				}
			}
			FRnInclusive(ref mut start, end, step_by) => {
				if (step_by > 0.0 && *start <= end) || (step_by < 0.0 && *start >= end) {
					let result = *start;
					*start += step_by;
					Some(Ok(Slot::Flo(result)))
				} else {
					None
				}
			}
			FRnOpen(ref mut start, step_by) => {
				let result = *start;
				*start += step_by;
				Some(Ok(Slot::Flo(result)))
			}
			ArrElements(ref arr, ref mut start_offs, back_offs) => {
				if *start_offs + back_offs < arr.len() as u32 {
					let result = arr.get(*start_offs);
					*start_offs += 1;
					Some(result)
				} else {
					None
				}
			}
			StrElements(ref st, ref mut start_offs, back_offs) => {
				if *start_offs + back_offs < st.len() as u32 {
					let result = st.get(*start_offs);
					*start_offs += 1;
					Some(result)
				} else {
					None
				}
			}
			TabEntries(ref remaining) | TabKeys(ref remaining) | TabValues(ref remaining) => {
				if remaining.len() > 0 {
					Some(remaining.pop())
				} else {
					None
				}
			}
			CoroResults(ref coro) => {
				if matches!(coro.state(), CoroState::Finished | CoroState::Poisoned) {
					None
				} else {
					let val = match glsp::coro_run(&coro.root(), None) {
						Ok(val) => val,
						Err(error) => return Some(Err(error))
					};
					if matches!(coro.state(), CoroState::Finished) {
						None
					} else {
						Some(Ok(Slot::from_val(&val)))
					}
				}
			}
			Once1(ref slot) => {
				let result = slot.clone();
				*state_ref = Empty;
				Some(Ok(result))
			}
			OnceN(ref remaining) => {
				if remaining.len() > 0 {
					Some(remaining.pop_start())
				} else {
					None
				}
			}
			OnceWith(ref gc_callable) => {
				let result = glsp::call(&gc_callable.root(), &()).map(|val| Slot::from_val(&val));
				*state_ref = Empty;
				Some(result)
			}
			Repeat1(ref slot) => {
				Some(Ok(slot.clone()))
			}
			RepeatN(ref arr, ref mut i, _) => {
				let element: Slot = arr.get(*i).unwrap();
				*i += 1;
				if *i >= arr.len() as u32 {
					*i = 0;
				}
				Some(Ok(element))
			}
			RepeatWith(ref gc_callable) => {
				Some(glsp::call(&gc_callable.root(), &()).map(|val| Slot::from_val(&val)))
			}
			AccessArr(ref arr, ref giter) => {
				let item = giter.raw_next();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Int(i) => Some(arr.get(i)),
						slot => Some(Err(error!("arr indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessStr(ref st, ref giter) => {
				let item = giter.raw_next();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Int(i) => Some(st.get(i)),
						slot => Some(Err(error!("str indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessObj(ref obj, ref giter) => {
				let item = giter.raw_next();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Sym(key) => Some(obj.get(key)),
						slot => Some(Err(error!("obj indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessRData(ref rdata, ref giter) => {
				let item = giter.raw_next();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Sym(key) => Some(rdata.get(key)),
						slot => Some(Err(error!("rdata indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessClass(ref class, ref giter) => {
				let item = giter.raw_next();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Sym(key) => Some(class.get(key)),
						slot => Some(Err(error!("class indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			Chunks(chunk_len, ref arr) => {
				if arr.len() == 0 {
					None
				} else {
					let len = min(arr.len(), chunk_len as usize);
					let chunk = glsp::arr_with_capacity(len);
					for _ in 0 .. len {
						chunk.push(arr.pop_start::<Slot>().unwrap()).unwrap();
					}
					Some(Ok(Slot::Arr(chunk.to_gc())))
				}
			}
			RChunks(chunk_len, ref arr) => {
				if arr.len() == 0 {
					None
				} else {
					let len = min(arr.len(), chunk_len as usize);
					let chunk = glsp::arr_with_capacity(len);
					for _ in 0 .. len {
						chunk.push_start(arr.pop::<Slot>().unwrap()).unwrap();
					}
					Some(Ok(Slot::Arr(chunk.to_gc())))
				}
			}
			Windows(window_len, ref arr) => {
				if arr.len() < window_len as usize {
					None
				} else {
					let window = glsp::arr_with_capacity(window_len as usize);
					for i in 0 .. window_len {
						window.push(arr.get::<Slot>(i).unwrap()).unwrap();
					}
					arr.pop_start::<Slot>().unwrap();
					Some(Ok(Slot::Arr(window.to_gc())))
				}
			}
			Lines(ref st) => {
				if st.len() == 0 {
					None
				} else {
					let accum = glsp::str();
					loop {
						let len = st.len();

						if len == 0 {
							break
						}

						let first: char = st.get(0).unwrap();
						if first == '\n' {
							st.pop_start::<char>().unwrap();
							break
						}

						if first == '\r' && len >= 2 && st.get::<char>(1).unwrap() == '\n' {
							st.pop_start::<char>().unwrap();
							st.pop_start::<char>().unwrap();
							break
						}

						accum.push(st.pop_start::<char>().unwrap()).unwrap();
					}

					Some(Ok(Slot::Str(accum.to_gc())))
				}
			}
			Split(ref src, ref split_at) => {
				while src.len() > 0 {
					let first = src.get::<char>(0).unwrap();
					if !split_at.iter().any(|ch| ch == first) {
						break
					}

					src.pop_start::<char>().unwrap();
				}

				if src.len() == 0 {
					None
				} else {
					let accum = glsp::str();

					while src.len() > 0 {
						let first = src.get::<char>(0).unwrap();
						if split_at.iter().any(|ch| ch == first) {
							break
						}

						accum.push(src.pop_start::<char>().unwrap()).unwrap();
					}

					Some(Ok(Slot::Str(accum.to_gc())))
				}
			},
			Rev(ref base) => base.raw_next_back(),
			Enumerate(ref base, ref mut n) => {
				match base.raw_next() {
					Some(Ok(slot)) => {
						let return_n = *n;
						*n += 1;
						Some(Ok(Slot::Arr(arr![return_n, slot].to_gc())))
					}
					err_or_none => err_or_none
				}		
			}
			Cloned(ref base) => {
				match base.raw_next() {
					Some(Ok(slot)) => {
						match slot.root().shallow_clone() {
							Ok(cloned) => Some(Ok(Slot::from_val(&cloned))),
							Err(err) => Some(Err(err))
						}
					}
					err_or_none => err_or_none
				}
			}	
			DeepCloned(ref base) => {
				match base.raw_next() {
					Some(Ok(slot)) => {
						match slot.root().deep_clone() {
							Ok(cloned) => Some(Ok(Slot::from_val(&cloned))),
							Err(err) => Some(Err(err))
						}
					}
					err_or_none => err_or_none
				}
			}
			StepBy(step_by, ref base) => { 
				match base.raw_next() {
					Some(Ok(slot)) => {
						let mut result = Some(Ok(slot));
						for _ in 0 .. step_by - 1 {
							if let Some(Err(err)) = base.raw_next() {
								result = Some(Err(err));
								break
							}
						}
						result
					}
					err_or_none => err_or_none
				}
			}
			Map(ref gc_callable, ref base) => {
				match base.raw_next() {
					Some(Ok(slot)) => {
						let result = glsp::call(&gc_callable.root(), &[slot]);
						Some(result.map(|val| Slot::from_val(&val)))
					}
					err_or_none => err_or_none
				}
			}
			Filter(ref gc_callable, ref base) => {
				loop {
					match base.raw_next() {
						Some(Ok(slot)) => {
							let result: Val = match glsp::call(&gc_callable.root(), (&slot,)) {
								Ok(val) => val,
								Err(err) => break Some(Err(err))
							};

							if result.is_truthy() {
								break Some(Ok(slot))
							}
						}
						err_or_none => break err_or_none
					}
				}
			}
			Zip(ref arr) => {
				let item = glsp::arr_with_capacity(arr.len());
				let mut result = Some(Ok(Slot::Arr(item.to_gc())));
				for val in arr.iter() {
					let giter = val.unwrap_giter();
					match giter.raw_next() {
						Some(Ok(slot)) => item.push(slot).unwrap(),
						err_or_none => {
							result = err_or_none;
							break
						}
					}
				}
				result
			}
			Chain(ref arr) => {
				loop {
					if arr.len() == 0 {
						break None
					}

					let giter: Root<GIter> = arr.get(0).unwrap();
					match giter.raw_next() {
						None => { arr.pop_start::<Slot>().unwrap(); }
						item_or_err => break item_or_err
					}
				}
			}
			Flatten(ref base, ref mut cur_giter) => {
				loop {
					if (*cur_giter).is_none() {
						match base.raw_next() {
							Some(Ok(slot)) => {
								let val = slot.root();
								if !val.is_iterable() {
									break Some(Err(error!("flatten received {}", 
									                       val.a_type_name())))
								}

								let iterable = Iterable::from_val(&val).unwrap();
								let new_giter = iterable.giter().to_gc();
								(*cur_giter) = Some(new_giter.clone());
								self.write_barrier(&new_giter);
							}
							none_or_err => break none_or_err
						}
					}

					match (*cur_giter).as_ref().unwrap().raw_next() {
						Some(result) => break Some(result),
						None => (*cur_giter) = None
					}
				}
			}
			Cycle(ref mut base, ref collection, ref mut next_i) => {
				let mut result = None;

				if base.is_some() {
					match base.as_ref().unwrap().raw_next() {
						None => (*base) = None,
						Some(Ok(slot)) => {
							collection.push(slot.clone()).unwrap();
							result = Some(Ok(slot));
						}
						err => result = err
					}
				}

				if base.is_none() && collection.len() > 0 {
					result = Some(Ok(collection.get::<Slot>(*next_i).unwrap()));
					*next_i = (*next_i + 1) % collection.len() as u32;
				}

				result
			}
			Take(ref mut remaining, ref base) => {
				if *remaining == 0 {
					None
				} else {
					*remaining -= 1;
					base.raw_next()
				}
			}
			TakeWhile(ref gc_callable, ref base) => {
				match base.raw_next() {
					Some(Ok(item)) => {
						let result: GResult<Val> = glsp::call(&gc_callable.root(), (&item,));
						match result {
							Ok(val) => {
								if val.is_truthy() {
									Some(Ok(item))
								} else {
									None
								}
							}
							Err(err) => Some(Err(err))
						}
					}
					err_or_none => err_or_none
				}
			}
			Skip(ref mut remaining, ref base) => {
				loop {
					if *remaining == 0 {
						break base.raw_next()
					}

					match base.raw_next() {
						None => {
							*remaining = 0;
							break None
						}
						Some(Ok(_)) => *remaining -= 1,
						Some(Err(err)) => break Some(Err(err))
					}
				}
			}
			SkipWhile(ref gc_callable, ref base) => {
				if let Some(gc_callable) = gc_callable {
					loop {
						match base.raw_next() {
							Some(Ok(item)) => {
								let result: GResult<Val> = glsp::call(
									&gc_callable.root(),
									(&item,)
								); 

								match result {
									Ok(val) => {
										if val.is_falsy() {
											break Some(Ok(item))
										}
									}
									Err(err) => break Some(Err(err))
								}
							}
							err_or_none => break err_or_none
						}
					}
				} else {
					base.raw_next()
				}
			}
		};

		if result.is_none() {
			*state_ref = Finished;
		}

		result
	}

	pub(crate) fn raw_next_back(&self) -> Option<GResult<Slot>> {
		use GIterState::*;
		
		let mut state_ref = self.state.borrow_mut();
		let result = match *state_ref {
			Finished | Empty => None,
			RnExclusive(start, ref mut end, step_by) => {
				if step_by > 0 && start < *end {
					let diff = (*end - start) - 1;
					let result = start + (diff - diff % step_by);

					*end -= step_by;
					Some(Ok(Slot::Int(result)))
				} else if step_by < 0 && start > *end {
					let diff = (start - *end) - 1;
					let result = start - (diff - diff % step_by);

					*end -= step_by;
					Some(Ok(Slot::Int(result)))
				} else {
					None
				}
			}
			RnInclusive(start, ref mut end, step_by) => {
				if step_by > 0 && start <= *end {
					let diff = *end - start;
					let result = start + (diff - diff % step_by);

					*end -= step_by;
					Some(Ok(Slot::Int(result)))
				} else if step_by < 0 && start >= *end {
					let diff = start - *end;
					let result = start - (diff - diff % step_by);

					*end -= step_by;
					Some(Ok(Slot::Int(result)))
				} else {
					None
				}
			}
			FRnExclusive(..) => {
				Some(Err(error!("floating-point ranges are not double-ended")))
			}
			FRnInclusive(..) => {
				Some(Err(error!("floating-point ranges are not double-ended")))
			}
			RnOpen(..) => {
				Some(Err(error!("half-open ranges are not double-ended")))
			}
			FRnOpen(..) => {
				Some(Err(error!("half-open ranges are not double-ended")))
			}
			ArrElements(ref arr, start_offs, ref mut back_offs) => {
				let len = arr.len() as u32;
				if start_offs + *back_offs < len {
					let result = arr.get(len - (*back_offs + 1));
					*back_offs += 1;
					Some(result)
				} else {
					None
				}
			}
			StrElements(ref st, start_offs, ref mut back_offs) => {
				let len = st.len() as u32;
				if start_offs + *back_offs < len {
					let result = st.get(len - (*back_offs + 1));
					*back_offs += 1;
					Some(result)
				} else {
					None
				}
			}
			TabEntries(_) | TabKeys(_) | TabValues(_) => {
				Some(Err(error!("table iterators are not double-ended")))
			}
			CoroResults(_) => {
				Some(Err(error!("coro iterators are not double-ended")))
			}
			Once1(ref slot) => {
				let result = slot.clone();
				*state_ref = Empty;
				Some(Ok(result))
			}
			OnceN(ref remaining) => {
				if remaining.len() > 0 {
					Some(remaining.pop())
				} else {
					None
				}
			}
			OnceWith(ref gc_callable) => {
				let result = glsp::call(&gc_callable.root(), &()).map(|val| Slot::from_val(&val));
				*state_ref = Empty;
				Some(result)
			}
			Repeat1(ref slot) => {
				Some(Ok(slot.clone()))
			}
			RepeatN(ref arr, _, ref mut back_i) => {
				let element: Slot = arr.get(*back_i).unwrap();
				if *back_i == 0 {
					*back_i = (arr.len() as u32) - 1;
				} else {
					*back_i -= 1;
				}
				Some(Ok(element))
			}
			RepeatWith(_) => {
				Some(Err(error!("repeat-with iterators are not double-ended")))
			}
			AccessArr(ref arr, ref giter) => {
				let item = giter.raw_next_back();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Int(i) => Some(arr.get(i)),
						slot => Some(Err(error!("arr indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessStr(ref st, ref giter) => {
				let item = giter.raw_next_back();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Int(i) => Some(st.get(i)),
						slot => Some(Err(error!("str indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessObj(ref obj, ref giter) => {
				let item = giter.raw_next_back();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Sym(key) => Some(obj.get(key)),
						slot => Some(Err(error!("obj indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessRData(ref rdata, ref giter) => {
				let item = giter.raw_next_back();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Sym(key) => Some(rdata.get(key)),
						slot => Some(Err(error!("rdata indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			AccessClass(ref class, ref giter) => {
				let item = giter.raw_next_back();
				if let Some(Ok(item)) = item {
					match item {
						Slot::Sym(key) => Some(class.get(key)),
						slot => Some(Err(error!("class indexed with {}", slot.a_type_name())))
					}
				} else {
					item
				}
			}
			Chunks(chunk_len, ref arr) => {
				if arr.len() == 0 {
					None
				} else {
					let fract = arr.len() % chunk_len as usize;
					let len = if fract == 0 { chunk_len as usize } else { fract };

					let chunk = glsp::arr_with_capacity(len as usize);
					for _ in 0 .. len {
						chunk.push_start(arr.pop::<Slot>().unwrap()).unwrap();
					}
					Some(Ok(Slot::Arr(chunk.to_gc())))
				}
			}
			RChunks(chunk_len, ref arr) => {
				if arr.len() == 0 {
					None
				} else {
					let fract = arr.len() % chunk_len as usize;
					let len = if fract == 0 { chunk_len as usize } else { fract };

					let chunk = glsp::arr_with_capacity(len as usize);
					for _ in 0 .. len {
						chunk.push(arr.pop_start::<Slot>().unwrap()).unwrap();
					}
					Some(Ok(Slot::Arr(chunk.to_gc())))
				}
			}
			Windows(window_len, ref arr) => {
				if arr.len() < window_len as usize {
					None
				} else {
					let window = glsp::arr_with_capacity(window_len as usize);
					for i in 1 .. window_len + 1 {
						window.push_start(arr.get::<Slot>(-(i as i32)).unwrap()).unwrap();
					}
					arr.pop::<Slot>().unwrap();
					Some(Ok(Slot::Arr(window.to_gc())))
				}
			}
			Lines(ref st) => {
				if st.len() == 0 {
					None
				} else {
					//the way that Lines iterates forwards is consistent with Rust: an empty
					//string has no lines. otherwise, it consumes any number of substrings 
					//followed by line endings, then consumes all remaining characters.

					//to replicate this when iterating backwards, if the string ends with a
					//line ending we pop it, then we consume characters until we encounter another
					//line ending, which we don't yet pop.

					let accum = glsp::str();

					if st.get::<char>(-1).unwrap() == '\n' {
						st.pop::<char>().unwrap();

						if st.len() >= 1 && st.get::<char>(-1).unwrap() == '\r' {
							st.pop::<char>().unwrap();
						}
					}

					loop {
						if st.len() == 0 {
							break
						}

						if st.get::<char>(-1).unwrap() == '\n' {
							break
						}

						let last = st.pop::<char>().unwrap();
						accum.push_start(last).unwrap();
					}

					Some(Ok(Slot::Str(accum.to_gc())))
				}
			}
			Split(ref src, ref split_at) => {
				while src.len() > 0 {
					let first = src.get::<char>(-1).unwrap();
					if !split_at.iter().any(|ch| ch == first) {
						break
					}

					src.pop::<char>().unwrap();
				}

				if src.len() == 0 {
					None
				} else {
					let accum = glsp::str();

					while src.len() > 0 {
						let first = src.get::<char>(-1).unwrap();
						if split_at.iter().any(|ch| ch == first) {
							break
						}

						accum.push_start(src.pop::<char>().unwrap()).unwrap();
					}

					Some(Ok(Slot::Str(accum.to_gc())))
				}
			}
			Rev(ref base) => base.raw_next(),
			Enumerate(_, _) => {
				Some(Err(error!("enumerate iterators are not double-ended")))	
			}
			Cloned(ref base) => {
				match base.raw_next_back() {
					Some(Ok(slot)) => {
						match slot.root().shallow_clone() {
							Ok(cloned) => Some(Ok(Slot::from_val(&cloned))),
							Err(err) => Some(Err(err))
						}
					}
					err_or_none => err_or_none
				}
			}	
			DeepCloned(ref base) => {
				match base.raw_next_back() {
					Some(Ok(slot)) => {
						match slot.root().deep_clone() {
							Ok(cloned) => Some(Ok(Slot::from_val(&cloned))),
							Err(err) => Some(Err(err))
						}
					}
					err_or_none => err_or_none
				}
			}
			StepBy(_, _) => { 
				Some(Err(error!("step-by iterators are not double-ended")))
			}
			Map(ref gc_callable, ref base) => {
				match base.raw_next_back() {
					Some(Ok(slot)) => {
						let result = glsp::call(&gc_callable.root(), &[slot]);
						Some(result.map(|val| Slot::from_val(&val)))
					}
					err_or_none => err_or_none
				}
			}
			Filter(ref gc_callable, ref base) => {
				loop {
					match base.raw_next_back() {
						Some(Ok(slot)) => {
							let result: Val = match glsp::call(&gc_callable.root(), (&slot,)) {
								Ok(val) => val,
								Err(err) => break Some(Err(err))
							};

							if result.is_truthy() {
								break Some(Ok(slot))
							}
						}
						err_or_none => break err_or_none
					}
				}
			}
			Zip(_) => {
				Some(Err(error!("zip iterators are not double-ended")))	
			}
			Chain(ref arr) => {
				loop {
					if arr.len() == 0 {
						break None
					}

					let giter: Root<GIter> = arr.get(-1).unwrap();
					match giter.raw_next_back() {
						None => { arr.pop::<Slot>().unwrap(); }
						item_or_err => break item_or_err
					}
				}
			}
			Flatten(_, _) => {
				Some(Err(error!("flatten iterators are not double-ended")))	
			}
			Cycle(_, _, _) => {
				Some(Err(error!("cycle iterators are not double-ended")))	
			}
			Take(_, _) => {
				Some(Err(error!("take iterators are not double-ended")))	
			}
			TakeWhile(_, _) => {
				Some(Err(error!("take-while iterators are not double-ended")))	
			}
			Skip(_, _) => {
				Some(Err(error!("skip iterators are not double-ended")))	
			}
			SkipWhile(_, _) => {
				Some(Err(error!("skip-while iterators are not double-ended")))	
			}
		};

		if result.is_none() {
			*state_ref = Finished;
		}

		result
	}

	pub(crate) fn state_name(&self) -> &'static str {
		use GIterState::*;

		match *self.state.borrow_mut() {
			Finished => "finished",
			Empty => "empty",
			RnExclusive(..) | RnOpen(..) | FRnExclusive(..) | FRnOpen(..) => "rn",
			RnInclusive(..) | FRnInclusive(..) => "rni",
			ArrElements(..) => "arr",
			StrElements(..) => "str",
			TabEntries(..) => "tab",
			TabKeys(..) => "keys",
			TabValues(..) => "values",
			CoroResults(..) => "coro",
			Once1(_) | OnceN(_) => "once",
			OnceWith(_) => "once-with",
			Repeat1(_) | RepeatN(_, _, _) => "repeat",
			RepeatWith(_) => "repeat-with",
			AccessArr(_, _) => "access-arr",
			AccessStr(_, _) => "access-str",
			AccessObj(_, _) => "access-obj",
			AccessRData(_, _) => "access-rdata",
			AccessClass(_, _) => "access-class",
			Chunks(_, _) => "chunks",
			RChunks(_, _) => "rchunks",
			Windows(_, _) => "windows",
			Lines(_) => "lines",
			Split(_, _) => "split",
			Rev(_) => "rev",
			Enumerate(_, _) => "enumerate",
			Cloned(_) => "cloned",
			DeepCloned(_) => "deep-cloned",
			StepBy(_, _) => "step-by",
			Map(_, _) => "map",
			Filter(_, _) => "filter",
			Zip(_) => "zip",
			Chain(_) => "chain",
			Flatten(_, _) => "flatten",
			Cycle(_, _, _) => "cycle",
			Take(_, _) => "take",
			TakeWhile(_, _) => "take-while",
			Skip(_, _) => "skip",
			SkipWhile(_, _) => "skip-while",
		}
	}
}

/**
The return value for [`GIter::len`](struct.GIter.html#method.len).
*/

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum GIterLen {
	Exact(usize),
	Infinite,
	Unknown
}

//so that we don't have to track owned_memory_usage, we prefer Gc<Arr> over Vec<Slot> for storing
//owned data. this has the added benefits of keeping the GIter struct small and making the
//recycler more effective.
#[derive(Clone)]
pub(crate) enum GIterState {

	//when iter_next() is called for an iterator which has no more items, it returns None
	//and replaces its state with Finished. this is also the state we use for GIters which are
	//waiting to be recycled.
	Finished,

	//the Empty state is for iterators which do not want to produce any more items (any calls to
	//iter_next() will return None), but do not want their previous item to be discarded (calls
	//to is_finished() will return false). without this, we would need a special "exhausted"
	//flag for states like Once1.
	Empty,

	RnExclusive(i32, i32, i32), //start, end, step_by
	RnInclusive(i32, i32, i32), //start, end, step_by
	RnOpen(i32, i32), //start, step_by

	FRnExclusive(f32, f32, f32),
	FRnInclusive(f32, f32, f32),
	FRnOpen(f32, f32),

	ArrElements(Gc<Arr>, u32, u32), //arr, start_offs, back_offs
	StrElements(Gc<Str>, u32, u32), //str, start_offs, back_offs
	TabEntries(Gc<Arr>),
	TabKeys(Gc<Arr>),
	TabValues(Gc<Arr>),
	CoroResults(Gc<Coro>),

	Once1(Slot),
	OnceN(Gc<Arr>),
	OnceWith(GcCallable),
	Repeat1(Slot),
	RepeatN(Gc<Arr>, u32, u32), //elems, next_i, next_back_i
	RepeatWith(GcCallable),

	AccessArr(Gc<Arr>, Gc<GIter>),
	AccessStr(Gc<Str>, Gc<GIter>),
	AccessObj(Gc<Obj>, Gc<GIter>),
	AccessRData(Gc<RData>, Gc<GIter>),
	AccessClass(Gc<Class>, Gc<GIter>),

	//todo: have Chunks, RChunks, Windows, Lines and Split stream the deque's contents 
	//in like ArrElements, rather than shallow-cloning the source when they're constructed
	Chunks(u32, Gc<Arr>),
	RChunks(u32, Gc<Arr>),
	Windows(u32, Gc<Arr>),

	Lines(Gc<Str>),
	Split(Gc<Str>, Gc<Str>), //src, split_at

	Rev(Gc<GIter>),
	Enumerate(Gc<GIter>, u32),
	Cloned(Gc<GIter>),
	DeepCloned(Gc<GIter>),
	StepBy(u32, Gc<GIter>),
	Map(GcCallable, Gc<GIter>),
	Filter(GcCallable, Gc<GIter>),
	Zip(Gc<Arr>),
	Chain(Gc<Arr>),
	Flatten(Gc<GIter>, Option<Gc<GIter>>), //base, cur_iter
	Cycle(Option<Gc<GIter>>, Gc<Arr>, u32), //base, collection, next_i
	Take(u32, Gc<GIter>), //remaining, base
	TakeWhile(GcCallable, Gc<GIter>),
	Skip(u32, Gc<GIter>), //remaining, base
	SkipWhile(Option<GcCallable>, Gc<GIter>),
}

impl GIterState {
	fn shallow_clone(&self) -> GIterState {
		use GIterState::*;

		//we perform just enough copying to ensure that result.raw_next() won't mutate `self`,
		//taking care not to clone a non-owned arr/str/tab which is being iterated.
		match self {
			TabEntries(arr) => TabEntries(arr.shallow_clone().to_gc()),
			TabKeys(arr) => TabKeys(arr.shallow_clone().to_gc()),
			TabValues(arr) => TabValues(arr.shallow_clone().to_gc()),

			//can't do anything for CoroResults as yet (todo?), because coros can't be
			//shallow-cloned. 

			AccessArr(arr, giter) => AccessArr(arr.clone(), giter.shallow_clone().to_gc()),
			AccessStr(st, giter) => AccessStr(st.clone(), giter.shallow_clone().to_gc()),
			AccessObj(ob, giter) => AccessObj(ob.clone(), giter.shallow_clone().to_gc()),
			AccessRData(rd, giter) => AccessRData(rd.clone(), giter.shallow_clone().to_gc()),
			AccessClass(cl, giter) => AccessClass(cl.clone(), giter.shallow_clone().to_gc()),

			Chunks(len, arr) => Chunks(*len, arr.shallow_clone().to_gc()),
			RChunks(len, arr) => RChunks(*len, arr.shallow_clone().to_gc()),
			Windows(len, arr) => Windows(*len, arr.shallow_clone().to_gc()),

			Lines(st) => Lines(st.shallow_clone().to_gc()),
			Split(src, split_at) => Split(src.shallow_clone().to_gc(), split_at.clone()),

			Rev(base) => Rev(base.shallow_clone().to_gc()),
			Enumerate(base, n) => Enumerate(base.shallow_clone().to_gc(), *n),
			Cloned(base) => Cloned(base.shallow_clone().to_gc()),
			DeepCloned(base) => DeepCloned(base.shallow_clone().to_gc()),
			StepBy(n, base) => StepBy(*n, base.shallow_clone().to_gc()),
			Map(callable, base) => Map(callable.clone(), base.shallow_clone().to_gc()),
			Filter(callable, base) => Filter(callable.clone(), base.shallow_clone().to_gc()),
			Zip(arr) => Zip(arr.deep_clone().unwrap().to_gc()),
			Chain(arr) => Chain(arr.deep_clone().unwrap().to_gc()),
			Flatten(base, cur_iter) => {
				Flatten(
					base.shallow_clone().to_gc(), 
					cur_iter.as_ref().map(|cur_iter| cur_iter.shallow_clone().to_gc())
				)
			}
			Cycle(base, arr, n) => {
				Cycle(
					base.as_ref().map(|base| base.shallow_clone().to_gc()), 
					arr.shallow_clone().to_gc(), 
					*n
				)
			}
			Take(n, base) => Take(*n, base.shallow_clone().to_gc()),
			TakeWhile(callable, base) => TakeWhile(callable.clone(), base.shallow_clone().to_gc()),
			Skip(n, base) => Skip(*n, base.shallow_clone().to_gc()),
			SkipWhile(callable, base) => SkipWhile(callable.clone(), base.shallow_clone().to_gc()),

			//everything else falls back to the auto-derived Clone impl
			state => state.clone()
		}
	}
}

#[derive(Clone)]
pub(crate) enum GcCallable {
	RFn(Gc<RFn>),
	GFn(Gc<GFn>),
	Class(Gc<Class>),
}

impl GcCallable {
	pub(crate) fn from_callable(callable: &Callable) -> GcCallable {
		match callable {
			Callable::RFn(rfn) => GcCallable::RFn(Gc::from_root(rfn)),
			Callable::GFn(gfn) => GcCallable::GFn(Gc::from_root(gfn)),
			Callable::Class(class) => GcCallable::Class(Gc::from_root(class))
		}
	}

	pub(crate) fn root(&self) -> Callable {
		match self {
			GcCallable::RFn(rfn) => Callable::RFn(rfn.root()),
			GcCallable::GFn(gfn) => Callable::GFn(gfn.root()),
			GcCallable::Class(class) => Callable::Class(class.root())
		}
	}
}

fn visit_gc_callable<V: Visitor>(v: &mut V, gc_callable: &GcCallable) {
	match gc_callable {
		GcCallable::RFn(rfn) => v.visit_gc(rfn),
		GcCallable::GFn(gfn) => v.visit_gc(gfn),
		GcCallable::Class(class) => v.visit_gc(class)
	}
}


//-------------------------------------------------------------------------------------------------
// Iterable, IterableOps
//-------------------------------------------------------------------------------------------------

/**
A type-erased `iterable`.

All of the wrapped types, and the `Iterable` enum itself, implement the 
[`IterableOps` trait](trait.IterableOps.html), which can be used to construct a 
[`GIter`](struct.GIter.html).
*/

#[derive(Clone, Debug)]
pub enum Iterable {
	Arr(Root<Arr>),
	Str(Root<Str>),
	Tab(Root<Tab>),
	Coro(Root<Coro>),
	GIter(Root<GIter>)
}

impl PartialEq<Iterable> for Iterable {
	fn eq(&self, other: &Iterable) -> bool {
		match (self, other) {
			(Iterable::Arr(a0), Iterable::Arr(a1)) => Root::ptr_eq(a0, a1),
			(Iterable::Str(s0), Iterable::Str(s1)) => Root::ptr_eq(s0, s1),
			(Iterable::Tab(t0), Iterable::Tab(t1)) => Root::ptr_eq(t0, t1),
			(Iterable::Coro(c0), Iterable::Coro(c1)) => Root::ptr_eq(c0, c1),
			(Iterable::GIter(i0), Iterable::GIter(i1)) => Root::ptr_eq(i0, i1),
			_ => false
		}
	}
}

/**
The `iterable` abstract type.
*/

pub trait IterableOps {
	fn giter(&self) -> Root<GIter>;
}

impl IterableOps for Iterable {
	fn giter(&self) -> Root<GIter> {
		match self {
			Iterable::Arr(arr) => arr.giter(),
			Iterable::Str(st) => st.giter(),
			Iterable::Tab(tab) => tab.giter(),
			Iterable::Coro(coro) => coro.giter(),
			Iterable::GIter(iter) => iter.giter()
		}
	}
}

impl IterableOps for Root<Arr> {
	fn giter(&self) -> Root<GIter> {
		glsp::giter(GIterState::ArrElements(self.to_gc(), 0, 0))
	}
}

impl IterableOps for Root<Str> {
	fn giter(&self) -> Root<GIter> {
		glsp::giter(GIterState::StrElements(self.to_gc(), 0, 0))
	}
}

impl IterableOps for Root<Tab> {
	fn giter(&self) -> Root<GIter> {
		let arr = glsp::arr_with_capacity(self.len());
		for pair in self.entries().iter_to::<Slot, Slot>() {
			let (key, value) = pair.unwrap();
			arr.push(arr![key, value]).unwrap();
		}

		glsp::giter(GIterState::TabEntries(arr.to_gc()))
	}
}

impl IterableOps for Root<Coro> {
	fn giter(&self) -> Root<GIter> {
		glsp::giter(GIterState::CoroResults(self.to_gc()))
	}
}

impl IterableOps for Root<GIter> {
	fn giter(&self) -> Root<GIter> {
		(*self).clone()
	}
}

impl IterableOps for Gc<Arr> {
	fn giter(&self) -> Root<GIter> {
		glsp::giter(GIterState::ArrElements(self.clone(), 0, 0))
	}
}

impl IterableOps for Gc<Str> {
	fn giter(&self) -> Root<GIter> {
		glsp::giter(GIterState::StrElements(self.clone(), 0, 0))
	}
}

impl IterableOps for Gc<Tab> {
	fn giter(&self) -> Root<GIter> {
		self.root().giter()
	}
}

impl IterableOps for Gc<Coro> {
	fn giter(&self) -> Root<GIter> {
		glsp::giter(GIterState::CoroResults(self.clone()))
	}
}

impl IterableOps for Gc<GIter> {
	fn giter(&self) -> Root<GIter> {
		self.root()
	}
}
