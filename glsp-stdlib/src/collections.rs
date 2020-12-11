use glsp::{
	Arr, bail, Callable, Class, Deque, DequeAccess, DequeAccessRange, DequeOps, ensure, 
	EprWriter, FromVal, GIterLen, GResult, IntoVal, Iterable, IterableOps, Obj,
	Parser, PrWriter, RData, Rest, RGlobal, Root, stock_syms::*, Str, Tab, Val
};
use glsp_proc_macros::{backquote};
use smallvec::{SmallVec};
use std::{fmt, io, str};
use std::cmp::{Ordering};
use std::io::{Write};
use std::iter::{FromIterator, repeat};
use super::{Std};

pub fn init(_sandboxed: bool) -> GResult<()> {
	//apis shared between several collection types
	glsp::bind_rfn("len", &len)?;
	glsp::bind_rfn("empty?", &emptyp)?;
	glsp::bind_rfn_macro("empty?", &emptyp_macro)?;
	glsp::bind_rfn("clear!", &clear)?;
	glsp::bind_rfn("access", &access)?;
	glsp::bind_rfn("access=", &set_access)?;
	glsp::bind_rfn("access-opt", &access_opt)?;
	glsp::bind_rfn("access-opt=", &set_access_opt)?;
	glsp::bind_rfn("access-slice", &access_slice)?;
	glsp::bind_rfn("access-slice=", &set_access_slice)?;
	glsp::bind_rfn("has?", &hasp)?;
	glsp::bind_rfn("remove!", &remove)?;
	glsp::bind_rfn("remove-opt!", &remove_opt)?;
	glsp::bind_rfn("remove-slice!", &remove_slice)?;
	glsp::bind_rfn("del!", &del)?;
	glsp::bind_rfn("del-opt!", &del_opt)?;
	glsp::bind_rfn("del-slice!", &del_slice)?;
	glsp::bind_rfn("map-syntax", &map_syntax)?;

	//deque apis
	glsp::bind_rfn("arr", &arr)?;
	glsp::bind_rfn("arr-from-elem", &arr_from_elem)?;
	glsp::bind_rfn("push!", &push)?;
	glsp::bind_rfn("push-start!", &push_start)?;
	glsp::bind_rfn("pop!", &pop)?;
	glsp::bind_rfn("pop-start!", &pop_start)?;
	glsp::bind_rfn("insert!", &insert)?;
	glsp::bind_rfn("swap-remove!", &swap_remove)?;
	glsp::bind_rfn("swap-remove-start!", &swap_remove_start)?;
	glsp::bind_rfn("grow!", &grow)?;
	glsp::bind_rfn("shrink!", &shrink)?;
	glsp::bind_rfn("sort", &sort)?;
	glsp::bind_rfn("sort!", &sort_mut)?;
	glsp::bind_rfn("starts-with?", &starts_withp)?;
	glsp::bind_rfn("ends-with?", &ends_withp)?;
	glsp::bind_rfn("position", &position)?;
	glsp::bind_rfn("rposition", &rposition)?;
	glsp::bind_rfn("rev!", &rev_mut)?;
	glsp::bind_rfn("map!", &map_mut)?;
	glsp::bind_rfn("retain!", &retain)?;
	glsp::bind_rfn("join", &join)?;

	//string apis
	glsp::bind_rfn("str", &str)?;
	glsp::bind_rfn("template-str", &template_str)?;
	glsp::bind_rfn("pretty-str", &pretty_str)?;
	glsp::bind_rfn("parse", &parse)?;
	glsp::bind_rfn("parse-all", &parse_all)?;
	glsp::bind_rfn("parse-1", &parse_1)?;
	glsp::bind_rfn("unparse", &unparse)?;
	glsp::bind_rfn("pretty-unparse", &pretty_unparse)?;
	glsp::bind_rfn("pr", &pr)?;
	glsp::bind_rfn("prn", &prn)?;
	glsp::bind_rfn("pretty-prn", &pretty_prn)?;
	glsp::bind_rfn("epr", &epr)?;
	glsp::bind_rfn("eprn", &eprn)?;
	glsp::bind_rfn("pretty-eprn", &pretty_eprn)?;
	glsp::bind_rfn("uppercase", &uppercase)?;
	glsp::bind_rfn("lowercase", &lowercase)?;
	glsp::bind_rfn("replace", &replace)?;
	glsp::bind_rfn("trim", &trim)?;
	glsp::bind_rfn("trim-start", &trim_start)?;
	glsp::bind_rfn("trim-end", &trim_end)?;
	glsp::bind_rfn("pad", &pad)?;
	glsp::bind_rfn("pad-start", &pad_start)?;
	glsp::bind_rfn("pad-end", &pad_end)?;
	glsp::bind_rfn("whitespace?", &whitespacep)?;
	glsp::bind_rfn("contains?", &containsp)?;

	//table apis
	glsp::bind_rfn("tab", &tab)?;
	glsp::bind_rfn("extend!", &extend)?;

	Ok(())
}

fn len(arg: Val) -> GResult<Val> {
	let collection_len: usize = match arg {
		Val::Arr(arr) => arr.len(),
		Val::Str(st) => st.len(),
		Val::Tab(tab) => tab.len(),
		Val::GIter(giter) => {
			return giter.len().into_val()
		}
		arg => bail!("argument is {} rather than an arr, str or tab", arg.a_type_name())
	};

	collection_len.into_val()
}

fn emptyp(any: Val) -> GResult<bool> {
	match any {
		Val::Arr(arr) => Ok(arr.len() == 0),
		Val::Str(st) => Ok(st.len() == 0),
		Val::Tab(tab) => Ok(tab.len() == 0),
		Val::GIter(giter) => Ok(giter.len() == GIterLen::Exact(0)),
		_ => bail!("expected an arr, str, tab or iter")
	}
}

fn emptyp_macro(arg_form: Val) -> Val {
	backquote!("(== (len ~arg_form) 0)")
}

fn clear(arg: Val) -> GResult<Val> {
	match arg {
		Val::Arr(ref arr) => arr.clear()?,
		Val::Str(ref st) => st.clear()?,
		Val::Tab(ref tab) => tab.clear()?,
		arg => bail!("received {} rather than an arr, str or tab", arg.a_type_name())
	}

	Ok(arg)
}

fn hasp(arg: Val, key: Val) -> GResult<bool> {
	match arg {
		Val::Arr(arr) => {
			if let Val::Int(index) = key {
				let len = arr.len() as i32;
				let i = if index < 0 { len - index } else { index };
				Ok(i >= 0 && i < len)
			} else {
				Ok(false)
			}
		}
		Val::Tab(tab) => Ok(tab.has(&key)?),
		Val::Obj(obj) => {
			match key {
				Val::Sym(key_name) => Ok(obj.has(key_name)?),
				_ => Ok(false)
			}
		}
		Val::Class(class) => {
			match key {
				Val::Sym(key_name) => Ok(class.has(key_name)?),
				_ => Ok(false)
			}
		}
		_ => Ok(false)
	}
}

fn remove(coll: Val, key: Val) -> GResult<Val> {
	match coll {
		Val::Arr(arr) => {
			if let Val::Int(i) = key {
				arr.remove(i)
			} else {
				bail!("attempted to index an arr with {}", key.a_type_name())
			}
		}
		Val::Str(st) => {
			if let Val::Int(i) = key {
				st.remove(i)
			} else {
				bail!("attempted to index a str with {}", key.a_type_name())
			}
		}
		Val::Tab(tab) => tab.remove(&key),
		val => bail!("expected an arr, str or tab, but received {}", val.a_type_name())
	}	
}

fn remove_opt(coll: Val, index: Val) -> GResult<Option<Val>> {
	match coll {
		Val::Arr(arr) => {
			if let Val::Int(i) = index {
				if i < arr.len() as i32 && i >= -(arr.len() as i32) {
					return Ok(Some(arr.remove(i)?))
				}
			}

			Ok(None)
		}
		Val::Str(st) => {
			if let Val::Int(i) = index {
				if i < st.len() as i32 && i >= -(st.len() as i32) {
					return Ok(Some(st.remove(i)?))
				}
			}

			Ok(None)
		}
		Val::Tab(tab) => {
			tab.remove_if_present(index)
		}
		val => bail!("attempted to index {}", val.a_type_name())
	}
}

fn remove_slice(deq: Deque, i0: Option<i32>, i1: Option<i32>) -> GResult<Deque> {
	let result = access_slice(deq.clone(), i0, i1)?;

	let i0 = i0.unwrap_or(0);
	let i1 = i1.unwrap_or(deq.len() as i32);
	deq.del_slice(i0 .. i1)?;

	Ok(result)
}

fn del(coll: Val, key: Val) -> GResult<()> {
	match coll {
		Val::Arr(arr) => {
			if let Val::Int(i) = key {
				arr.del(i)
			} else {
				bail!("attempted to index an arr with {}", key.a_type_name())
			}
		}
		Val::Str(st) => {
			if let Val::Int(i) = key {
				st.del(i)
			} else {
				bail!("attempted to index a str with {}", key.a_type_name())
			}
		}
		Val::Tab(tab) => tab.del(&key),
		val => bail!("expected an arr, str or tab, but received {}", val.a_type_name())
	}	
}

fn del_opt(coll: Val, key: Val) -> GResult<()> {
	match coll {
		Val::Arr(arr) => {
			if let Val::Int(i) = key {
				if i < arr.len() as i32 && i >= -(arr.len() as i32) {
					arr.del(i)?;
				}
			}
		}
		Val::Str(st) => {
			if let Val::Int(i) = key {
				if i < st.len() as i32 && i >= -(st.len() as i32) {
					st.del(i)?;
				}
			}
		}
		Val::Tab(tab) => {
			tab.del_if_present(key)?;
		}
		val => bail!("attempted to index {}", val.a_type_name())
	}

	Ok(())
}

fn del_slice(deq: Deque, i0: Option<i32>, i1: Option<i32>) -> GResult<()> {
	let i0 = i0.unwrap_or(0);
	let i1 = i1.unwrap_or(deq.len() as i32);
	deq.del_slice(i0 .. i1)
}

fn access(coll: Val, index: Val) -> GResult<Val> {
	match coll {
		Val::Arr(arr) => {
			match index {
				Val::Int(i) => arr.get(i),
				Val::Arr(src) => Ok(Val::GIter(Arr::access_giter(&arr, &src.giter()))),
				Val::Str(src) => Ok(Val::GIter(Arr::access_giter(&arr, &src.giter()))),
				Val::Tab(src) => Ok(Val::GIter(Arr::access_giter(&arr, &src.giter()))),
				Val::Coro(src) => Ok(Val::GIter(Arr::access_giter(&arr, &src.giter()))),
				Val::GIter(src) => Ok(Val::GIter(Arr::access_giter(&arr, &src))),
				index => bail!("attempted to index an arr with {}", index.a_type_name())
			}
		}
		Val::Str(st) => {
			match index {
				Val::Int(i) => st.get(i),
				Val::Arr(src) => Ok(Val::GIter(Str::access_giter(&st, &src.giter()))),
				Val::Str(src) => Ok(Val::GIter(Str::access_giter(&st, &src.giter()))),
				Val::Tab(src) => Ok(Val::GIter(Str::access_giter(&st, &src.giter()))),
				Val::Coro(src) => Ok(Val::GIter(Str::access_giter(&st, &src.giter()))),
				Val::GIter(src) => Ok(Val::GIter(Str::access_giter(&st, &src))),
				index => bail!("attempted to index a str with {}", index.a_type_name())
			}
		}
		Val::Tab(tab) => {
			tab.get(index)
		}
		Val::Obj(obj) => {
			match index {
				Val::Sym(key_name) => obj.get(key_name),
				Val::Arr(src) => Ok(Val::GIter(Obj::access_giter(&obj, &src.giter()))),
				Val::Str(src) => Ok(Val::GIter(Obj::access_giter(&obj, &src.giter()))),
				Val::Tab(src) => Ok(Val::GIter(Obj::access_giter(&obj, &src.giter()))),
				Val::Coro(src) => Ok(Val::GIter(Obj::access_giter(&obj, &src.giter()))),
				Val::GIter(src) => Ok(Val::GIter(Obj::access_giter(&obj, &src))),
				index => bail!("attempted to index an obj with {}", index.a_type_name())
			}
		}
		Val::RData(rdata) => {
			match index {
				Val::Sym(key_name) => rdata.get(key_name),
				Val::Arr(src) => Ok(Val::GIter(RData::access_giter(&rdata, &src.giter()))),
				Val::Str(src) => Ok(Val::GIter(RData::access_giter(&rdata, &src.giter()))),
				Val::Tab(src) => Ok(Val::GIter(RData::access_giter(&rdata, &src.giter()))),
				Val::Coro(src) => Ok(Val::GIter(RData::access_giter(&rdata, &src.giter()))),
				Val::GIter(src) => Ok(Val::GIter(RData::access_giter(&rdata, &src))),
				index => bail!("attempted to index an rdata with {}", index.a_type_name())
			}
		}
		Val::Class(class) => {
			match index {
				Val::Sym(key_name) => class.get(key_name),
				Val::Arr(src) => Ok(Val::GIter(Class::access_giter(&class, &src.giter()))),
				Val::Str(src) => Ok(Val::GIter(Class::access_giter(&class, &src.giter()))),
				Val::Tab(src) => Ok(Val::GIter(Class::access_giter(&class, &src.giter()))),
				Val::Coro(src) => Ok(Val::GIter(Class::access_giter(&class, &src.giter()))),
				Val::GIter(src) => Ok(Val::GIter(Class::access_giter(&class, &src))),
				index => bail!("attempted to index a class with {}", index.a_type_name())
			}
		}
		val => bail!("attempted to index {}", val.a_type_name())
	}
}

fn set_access(coll: Val, index: Val, new_value: Val) -> GResult<()> {
	match coll {
		Val::Arr(arr) => {
			match index {
				Val::Int(i) => arr.set(i, new_value),
				index => bail!("attempted to index an arr with {}", index.a_type_name())
			}
		}
		Val::Str(st) => {
			match index {
				Val::Int(i) => st.set(i, new_value),
				index => bail!("attempted to index a str with {}", index.a_type_name())
			}
		}
		Val::Tab(tab) => {
			tab.set(index, new_value)
		}
		Val::Obj(obj) => {
			match index {
				Val::Sym(key_name) => obj.set(key_name, new_value),
				index => bail!("attempted to index an obj with {}", index.a_type_name())
			}
		}
		Val::RData(rdata) => {
			match index {
				Val::Sym(key_name) => rdata.set(key_name, new_value),
				index => bail!("attempted to index an rdata with {}", index.a_type_name())
			}
		}
		val => bail!("attempted to index {} for mutation", val.a_type_name())
	}
}

fn access_opt(coll: Val, index: Val) -> GResult<Option<Val>> {
	match coll {
		Val::Arr(arr) => {
			match index {
				Val::Int(i) => {
					if i < arr.len() as i32 && i >= -(arr.len() as i32) {
						Ok(Some(arr.get(i)?))
					} else {
						Ok(None)
					}
				}
				_ => Ok(None)
			}
		}
		Val::Str(st) => {
			match index {
				Val::Int(i) => {
					if i < st.len() as i32 && i >= -(st.len() as i32) {
						Ok(Some(st.get(i)?))
					} else {
						Ok(None)
					}
				}
				_ => Ok(None)
			}
		}
		Val::Tab(tab) => {
			tab.get_if_present(index)
		}
		Val::Obj(obj) => {
			match index {
				Val::Sym(key_name) => obj.get_if_present(key_name),
				_ => Ok(None)
			}
		}
		Val::RData(rdata) => {
			match index {
				Val::Sym(key_name) => rdata.get_if_present(key_name),
				_ => Ok(None)
			}
		}
		Val::Class(class) => {
			match index {
				Val::Sym(key_name) => class.get_if_present(key_name),
				_ => Ok(None)
			}
		}
		val => bail!("attempted to index {}", val.a_type_name())
	}
}

fn set_access_opt(coll: Val, index: Val, new_value: Val) -> GResult<()> {
	match coll {
		Val::Arr(arr) => {
			if let Val::Int(i) = index {
				if i < arr.len() as i32 && i >= -(arr.len() as i32) {
					arr.set(i, new_value)?;
				}
			}
		}
		Val::Str(st) => {
			if let Val::Int(i) = index {
				if i < st.len() as i32 && i >= -(st.len() as i32) {
					st.set(i, new_value)?;
				}
			}
		}
		Val::Tab(tab) => {
			tab.set_if_present(index, new_value)?;
		}
		Val::Obj(obj) => {
			if let Val::Sym(key_name) = index {
				obj.set_if_present(key_name, new_value)?;
			}
		}
		Val::RData(rdata) => {
			if let Val::Sym(key_name) = index {
				rdata.set_if_present(key_name, new_value)?;
			}
		}
		val => bail!("attempted to index {} for mutation", val.a_type_name())
	}

	Ok(())
}

fn arr(args: Rest<Val>) -> GResult<Root<Arr>> {
	glsp::arr_from_iter(args.iter())
}

fn arr_from_elem(elem: Val, reps: usize) -> GResult<Root<Arr>> {
	glsp::arr_from_elem(elem, reps)
}

fn push(deq: Deque, to_push: Rest<Val>) -> GResult<()> {
	deq.extend(to_push)
}

fn push_start(deq: Deque, to_push: Rest<Val>) -> GResult<()> {
	deq.reserve(to_push.len())?;
	for val in to_push.iter().rev() {
		deq.push_start(val)?;
	}

	Ok(())
}

fn pop(deq: Deque) -> GResult<Val> {
	deq.pop()
}

fn pop_start(deq: Deque) -> GResult<Val> {
	deq.pop_start()
}

fn grow(deq: Deque, start_to_add: usize, end_to_add: usize, fill: Option<Val>) -> GResult<()> {
	deq.grow(start_to_add, end_to_add, fill.unwrap_or(deq.fill()))
}

fn shrink(deq: Deque, start_to_remove: usize, end_to_remove: usize) -> GResult<()> {
	deq.shrink(start_to_remove, end_to_remove)
}

fn insert(deq: Deque, index: i32, vals: Rest<Val>) -> GResult<()> {
	let orig_len = deq.len();
	deq.grow(0, vals.len(), deq.fill())?;

	for i in (index .. orig_len as i32).rev() {
		deq.set(i + vals.len() as i32, deq.get::<Val>(i)?)?;
	}

	for (i, val) in vals.iter().enumerate() {
		deq.set(index + i as i32, val)?;
	}

	Ok(())
}

fn swap_remove(deq: Deque, index: i32) -> GResult<Val> {
	deq.swap_remove(index)
}

fn swap_remove_start(deq: Deque, index: i32) -> GResult<Val> {
	deq.swap_remove_start(index)
}

fn access_slice(
	deq: Deque,
	i0: Option<i32>,
	i1: Option<i32>
) -> GResult<Deque> {

	let len = deq.len() as i32;
	let i0 = i0.unwrap_or(0);
	let i1 = i1.unwrap_or(deq.len() as i32);
	let adj0 = if i0 < 0 { len + i0 } else { i0 };
	let adj1 = if i1 < 0 { len + i1 } else { i1 };

	ensure!(adj0 <= adj1 && adj0 >= 0 && adj0 <= len && adj1 >= 0 && adj1 <= len,
	        "invalid slice indexes: start {}, end {}", i0, i1);

	Deque::from_iter(&deq, (adj0 .. adj1).map(|i| deq.get::<Val>(i).unwrap()))
}

fn set_access_slice(
	deq: Deque,
	i0: Option<i32>,
	i1: Option<i32>,
	src: Iterable
) -> GResult<()> {

	//because self-assignment is possible, we need to collect the source Vals into a SmallVec.
	//todo: could we eliminate this copy when `src` is a Deque which is distinct from `deq`?
	let mut vec = SmallVec::<[Val; 32]>::new();
	let giter = match src {
		Iterable::Arr(arr) => {
			vec.extend(arr.iter());
			None
		}
		Iterable::Str(st) => {
			vec.extend(st.iter().map(Val::Char));
			None
		}
		Iterable::Tab(tab) => Some(tab.giter()),
		Iterable::Coro(coro) => Some(coro.giter()),
		Iterable::GIter(giter) => Some(giter)
	};

	if let Some(giter) = giter {
		vec.reserve(giter.size_hint().0);
		for item in giter {
			vec.push(item?);
		}
	}

	//perform the assignment
	let len = deq.len() as i32;
	let i0 = i0.unwrap_or(0);
	let i1 = i1.unwrap_or(deq.len() as i32);
	let adj0 = if i0 < 0 { len + i0 } else { i0 };
	let adj1 = if i1 < 0 { len + i1 } else { i1 };

	ensure!(adj0 <= adj1 && adj0 >= 0 && adj0 <= len && adj1 >= 0 && adj1 <= len,
	        "invalid slice indexes: start {}, end {}", i0, i1);

	let src_len = vec.len() as i32;
	let remove_len = adj1 - adj0;

	if src_len <= remove_len {
		for i in 0 .. src_len {
			deq.set(adj0 + i, vec[i as usize].clone())?;
		}

		//todo: we would ideally shift from the start rather than the end, when that would cause
		//fewer elements to be moved
		let offs = remove_len - src_len;
		if offs != 0 {
			for i in adj0 + src_len .. (len - offs) {
				deq.set(i, deq.get::<Val>(i + offs)?)?;
			}
			deq.shrink(0, offs as usize)?;
		}
	} else {
		let offs = src_len - remove_len;
		deq.grow(0, offs as usize, deq.fill())?;

		for i in (adj1 .. len).rev() {
			deq.set(i + offs, deq.get::<Val>(i)?)?;
		}

		for i in 0 .. src_len {
			deq.set(adj0 + i, vec[i as usize].clone())?;
		}
	}

	Ok(())
}

fn map_syntax(callable: Callable, coll: Val) -> GResult<Val> {
	match coll {
		Val::Arr(arr) => {
			let output = arr.shallow_clone();
			
			for i in 0 .. arr.len() {
				let before = output.get::<Val>(i)?;
				let after: Val = glsp::call(&callable, &[before])?;
				output.set(i, after)?;
			}

			Ok(Val::Arr(output))
		}
		Val::Tab(tab) => {
			let keys = SmallVec::<[Val; 8]>::from_iter(tab.entries().keys());

			let output = glsp::tab();

			for key_before in keys.iter().cloned() {
				let value_before: Val = tab.get(&key_before)?;

				let key_after: Val = glsp::call(&callable, &[key_before])?;
				let value_after: Val = glsp::call(&callable, &[value_before])?;

				output.set(key_after, value_after)?;
			}

			Ok(Val::Tab(output))
		}
		val => Ok(val)
	}
}

fn sort(deq: Deque, ord: Option<Callable>) -> GResult<Deque> {
	let cloned = deq.shallow_clone();
	sort_mut(cloned.clone(), ord)?;
	Ok(cloned)
}

fn sort_mut(deq: Deque, ord: Option<Callable>) -> GResult<()> {
	match ord {
		None => deq.sort(),
		Some(Callable::RFn(ref rfn))
			if Root::ptr_eq(rfn, Std::borrow().ord_rfn.as_ref().unwrap()) => deq.sort(),
		Some(ord) => {
			deq.sort_by(|v0, v1| {
				match glsp::call(&ord, (v0, v1,))? {
					Val::Sym(LT_SYM) => Ok(Ordering::Less),
					Val::Sym(NUM_EQ_SYM) => Ok(Ordering::Equal),
					Val::Sym(GT_SYM) => Ok(Ordering::Greater),
					result => bail!("expected <, == or >, but received {}", result),
				}
			})
		}
	}
}

fn starts_withp(deq: Deque, prefix: Deque) -> GResult<bool> {
	if deq.len() < prefix.len() {
		return Ok(false)
	}

	match (deq, prefix) {
		(Deque::Arr(arr), Deque::Arr(prefix)) => {
			for (i, val) in prefix.iter().enumerate() {
				if !arr.get::<Val>(i).unwrap().try_eq(&val)? {
					return Ok(false)
				}
			}
		}
		(Deque::Str(st), Deque::Str(prefix)) => {
			for (i, ch) in prefix.iter().enumerate() {
				if st.get::<char>(i).unwrap() != ch {
					return Ok(false)
				}
			}
		}
		_ => bail!("type-mismatch when comparing deques")
	}	

	Ok(true)
}

fn ends_withp(deq: Deque, suffix: Deque) -> GResult<bool> {
	if deq.len() < suffix.len() {
		return Ok(false)
	}

	match (deq, suffix) {
		(Deque::Arr(arr), Deque::Arr(suffix)) => {
			for (i, val) in suffix.iter().rev().enumerate() {
				if !arr.get::<Val>(-(i as isize + 1)).unwrap().try_eq(&val)? {
					return Ok(false)
				}
			}
		}
		(Deque::Str(st), Deque::Str(suffix)) => {
			for (i, ch) in suffix.iter().rev().enumerate() {
				if st.get::<char>(-(i as isize + 1)).unwrap() != ch {
					return Ok(false)
				}
			}
		}
		_ => bail!("type-mismatch when comparing deques")
	}	

	Ok(true)
}

fn position_impl(
	haystack: Deque,
	needle: Val,
	from: isize,
	to: isize,
	step: isize
) -> GResult<Option<usize>> {

	let mut i = from;

	//two overloads. if the haystack is an array, the needle must be a predicate callback.
	//if it's a string, the needle can be a char, or it can be a substring.
	match haystack {
		Deque::Arr(arr) => {
			let callable = Callable::from_val(&needle)?;

			while i != to {
				let result: Val = glsp::call(&callable, &[arr.get::<Val>(i)?,])?;
				if result.is_truthy() {
					return Ok(Some(i as usize))
				}

				i += step;
			}
		}
		Deque::Str(st) => {
			match needle {
				Val::Char(ch) => {
					while i != to {
						if st.get::<char>(i).unwrap() == ch {
							return Ok(Some(i as usize))
						}

						i += step;
					}
				}
				Val::Str(needle) => {
					ensure!(needle.len() > 0, "substring search requires a non-empty str");

					while i != to {
						let mut matches = false;

						if i <= (st.len() - needle.len()) as isize {
							matches = true;
							for j in 0 .. needle.len() as isize {
								if st.get::<char>(i+j).unwrap() != needle.get::<char>(j).unwrap() {
									matches = false;
									break
								}
							}
						}

						if matches {
							return Ok(Some(i as usize))
						}

						i += step;
					}
				}
				val => bail!("expected a char or str, received {}", val.a_type_name())
			}
		}
	}

	Ok(None)
}

fn position(haystack: Deque, needle: Val, from: Option<isize>) -> GResult<Option<usize>> {
	let mut from = from.unwrap_or(0);

	if from < 0 {
		from += haystack.len() as isize;
	}

	if from < 0 {
		from = 0;
	}

	if from >= haystack.len() as isize {
		return Ok(None)
	}

	let to = haystack.len() as isize;
	position_impl(haystack, needle, from, to, 1)
}

//we return a positive rather than negative index because it makes it easier to compare
//(rposition) results with (position) results
fn rposition(haystack: Deque, needle: Val, from: Option<isize>) -> GResult<Option<usize>> {
	let mut from = from.unwrap_or(haystack.len().saturating_sub(1) as isize);

	if from < 0 {
		from += haystack.len() as isize;
	}

	if from >= haystack.len() as isize {
		from = haystack.len().saturating_sub(1) as isize;
	}

	if from < 0 {
		return Ok(None)
	}

	position_impl(haystack, needle, from, -1, -1)
}

//we return #n from these fns for the same reason we return #n from (inc!): we want to gently
//discourage in-place mutation, and we don't want the user to use `rev!` rather than `rev`
//accidentally.
fn rev_mut(deque: Deque) -> GResult<()> {
	for i in 0 .. (deque.len() as isize) / 2 {
		deque.swap(i, (-i) - 1)?;
	}

	Ok(())
}

fn map_mut(callable: Callable, deque: Deque) -> GResult<()> {
	for i in 0 .. deque.len() {
		let before = deque.get::<Val>(i)?;
		let after: Val = glsp::call(&callable, &[before])?;
		deque.set(i, after)?;
	}

	Ok(())
}

fn retain(callable: Callable, deque: Deque) -> GResult<()> {
	let mut in_i = 0;
	let mut out_i = 0;

	while in_i < deque.len() {
		let val = deque.get::<Val>(in_i)?;
		let result: Val = glsp::call(&callable, (&val,))?;
		if result.is_truthy() {
			if in_i != out_i {
				deque.set(out_i, val)?;
			}
			out_i += 1;
		}

		in_i += 1;
	}

	if out_i < in_i {
		deque.shrink(0, in_i - out_i)?;
	}

	Ok(())
}

fn join(deques: Root<Arr>, glue: Option<Deque>) -> GResult<Deque> {
	match glue {
		Some(Deque::Arr(glue)) => {
			let output = glsp::arr();

			for (i, result) in deques.iter_to::<Root<Arr>>().enumerate() {
				for val in result?.iter() {
					output.push(val)?;
				}

				if i < deques.len() - 1 {
					for val in glue.iter() {
						output.push(val)?;
					}
				}
			}

			Ok(Deque::Arr(output))
		}
		Some(Deque::Str(glue)) => {
			let output = glsp::str();

			for (i, result) in deques.iter_to::<Root<Str>>().enumerate() {
				for ch in result?.iter() {
					output.push(ch)?;
				}

				if i < deques.len() - 1 {
					for ch in glue.iter() {
						output.push(ch)?;
					}
				}
			}

			Ok(Deque::Str(output))
		}
		None => {
			if deques.len() == 0 {
				Ok(Deque::Arr(glsp::arr()))
			} else {
				match deques.get::<Val>(0).unwrap() {
					Val::Arr(_) => {
						let output = glsp::arr();

						for result in deques.iter_to::<Root<Arr>>() {
							for val in result?.iter() {
								output.push(val)?;
							}
						}

						Ok(Deque::Arr(output))
					}
					Val::Str(_) => {
						let output = glsp::str();

						for result in deques.iter_to::<Root<Str>>() {
							for ch in result?.iter() {
								output.push(ch)?;
							}
						}

						Ok(Deque::Str(output))
					}
					first => bail!("expected a Deque, received {}", first.a_type_name())
				}
			}
		}
	}
}

pub(crate) fn build_msg<T>(mut dst: T, args: &[Val], insert_spaces: bool) -> fmt::Result 
where
	T: fmt::Write
{
	let mut prev_char_or_str = false;
	for (i, arg) in args.iter().enumerate() {
		let char_or_str = match arg {
			Val::Char(_) | Val::Str(_) => true,
			_ => false
		};

		if insert_spaces && i > 0 && !char_or_str && !prev_char_or_str {
			write!(dst, " ")?;
		}

		write!(dst, "{}", arg)?;

		prev_char_or_str = char_or_str;
	}

	Ok(())
}

fn str(args: Rest<Val>) -> Root<Str> {
	let mut st = glsp::str();
	build_msg(&mut st, &args, true).unwrap();
	st
}

fn template_str(args: Rest<Val>) -> Root<Str> {
	let mut st = glsp::str();
	build_msg(&mut st, &args, true).unwrap();
	st
}

fn pretty_str(arg: Val) -> Root<Str> {
	use std::fmt::Write;

	let mut st = glsp::str();
	write!(st, "{:#?}", arg).unwrap();
	st
}

fn parse(st: Root<Str>, filename: Option<&str>) -> GResult<Root<Arr>> {
	//we don't want to convert an entire Str to utf-8 just to process a small part of it, but
	//we also don't want to force the user to maintain state between calls. the compromise is that
	//we process one line of text at a time from the input Str, then stop when we produce a form.
	let file_id = filename.map(|name| glsp::filename(name));
	let mut line_buffer = String::new();

	let mut parser = Parser::new(file_id);

	let mut form: Option<Val> = None;
	let mut chars_read = 0;

	'outer: for ch in st.iter() {
		line_buffer.push(ch);
		if ch == '\n' {
			let mut input = line_buffer.as_str();
			let mut prev_len = input.len();
			while input.len() > 0 {
				let maybe_form = parser.parse(&mut input)?;
				chars_read += prev_len - input.len();

				if let Some(parsed_form) = maybe_form {
					form = Some(parsed_form);
					break 'outer
				}

				prev_len = input.len();
			}

			drop(input);
			line_buffer.clear();
		}
	}

	//produce the output arr
	match form {
		Some(form) => Ok(glsp::arr![OK_SYM, form, chars_read]),
		None => Ok(glsp::arr![END_OF_INPUT_SYM, Val::Nil, chars_read])
	}
}

fn parse_all(text: &str, filename: Option<&str>) -> GResult<Vec<Val>> {
	glsp::parse_all(text, filename)
}

fn parse_1(text: &str, filename: Option<&str>) -> GResult<Val> {
	glsp::parse_1(text, filename)
}

fn unparse(args: Rest<Val>) -> GResult<String> {
	use std::fmt::Write;
	
	let mut builder = String::new();

	for (i, arg) in args.iter().enumerate() {
		match arg.check_representability() {
			Ok(()) => write!(&mut builder, "{:?}", arg).unwrap(),
			Err(msg) => bail!("{}", msg)
		}

		if i < args.len() - 1 {
			write!(&mut builder, " ").unwrap()
		}
	}

	Ok(builder)
}

fn pretty_unparse(arg: Val) -> GResult<String> {
	match arg.check_representability() {
		Ok(()) => Ok(format!("{:#?}", arg)),
		Err(msg) => bail!("{}", msg)
	}
}

//an adapter for any io::Write which implements fmt::Write
struct IoFmtAdapter<'a, T: io::Write>(&'a mut T);

impl<'a, T: io::Write> fmt::Write for IoFmtAdapter<'a, T> {
	fn write_str(&mut self, st: &str) -> fmt::Result {
		self.0.write_all(st.as_bytes()).ok();
		Ok(())
	}
}

fn pr(args: Rest<Val>) {
	build_msg(IoFmtAdapter(&mut PrWriter), &*args, true).ok();
	PrWriter.flush().ok();
}

fn prn(args: Rest<Val>) {
	build_msg(IoFmtAdapter(&mut PrWriter), &*args, true).ok();
	writeln!(PrWriter).ok();
}

fn pretty_prn(arg: Val) {
	writeln!(PrWriter, "{:#}", arg).ok();
}

fn epr(args: Rest<Val>) {
	build_msg(IoFmtAdapter(&mut EprWriter), &*args, true).ok();
	EprWriter.flush().ok();
}

fn eprn(args: Rest<Val>) {
	build_msg(IoFmtAdapter(&mut EprWriter), &*args, true).ok();
	writeln!(EprWriter).ok();
}

fn pretty_eprn(arg: Val) {
	writeln!(EprWriter, "{:#}", arg).ok();
}

fn uppercase(st: &Str) -> GResult<Root<Str>> {
	glsp::str_from_iter(st.iter().map(char::to_uppercase).flatten())
}

fn lowercase(st: &Str) -> GResult<Root<Str>> {
	glsp::str_from_iter(st.iter().map(char::to_lowercase).flatten())
}

fn replace(st: &Str, before: &Str, after: &Str) -> GResult<Root<Str>> {
	ensure!(before.len() > 0, "'before' string must not be empty");

	if before.len() > st.len() {
		return Ok(st.shallow_clone())
	}

	let result = glsp::str();

	let mut start = 0;
	while start < st.len() - before.len() {
		let mut matches = true;
		for i in 0 .. before.len() {
			if st.get::<char>(start + i).unwrap() != before.get::<char>(i).unwrap() {
				matches = false;
				break
			}
		}

		if !matches {
			result.push(st.get::<char>(start).unwrap()).unwrap();
			start += 1;
		} else {
			for ch in after.iter() {
				result.push(ch).unwrap();
			}
			start += before.len();
		}
	}

	for i in start .. st.len() {
		result.push(st.get::<char>(i).unwrap()).unwrap();
	}

	Ok(result)
}

fn trim_impl(st: &Str, start: bool, end: bool, to_trim: Option<&Str>) -> GResult<Root<Str>> {
	let mut to_trim_buf = SmallVec::<[char; 16]>::new();
	if let Some(to_trim) = to_trim {
		to_trim_buf.extend(to_trim.iter());
	}

	let should_trim_char = |ch: char| -> bool {
		match to_trim {
			Some(_) => to_trim_buf.contains(&ch),
			None => ch.is_whitespace()
		}
	};

	let start_i = if start {
		let mut start_i = 0;
		while start_i < st.len() && should_trim_char(st.get(start_i).unwrap()) {
			start_i += 1;
		}
		start_i
	} else {
		0
	};

	let end_i = if end {
		let mut end_i = st.len();
		while end_i > start_i && should_trim_char(st.get(end_i - 1).unwrap()) {
			end_i -= 1;
		}
		end_i
	} else {
		st.len()
	};

	glsp::str_from_iter((start_i .. end_i).map(|i| st.get::<char>(i).unwrap()))
}

fn trim(st: &Str, to_trim: Option<&Str>) -> GResult<Root<Str>> {
	trim_impl(st, true, true, to_trim)
}

fn trim_start(st: &Str, to_trim: Option<&Str>) -> GResult<Root<Str>> {
	trim_impl(st, true, false, to_trim)
}

fn trim_end(st: &Str, to_trim: Option<&Str>) -> GResult<Root<Str>> {
	trim_impl(st, false, true, to_trim)
}

fn pad(st: &Str, len: usize, ch: Option<char>) -> GResult<Root<Str>> {
	pad_end(st, len, ch)
}

fn pad_start(st: &Str, len: usize, ch: Option<char>) -> GResult<Root<Str>> {
	let ch = ch.unwrap_or(' ');
	let to_pad = len.saturating_sub(st.len());

	glsp::str_from_iter(repeat(ch).take(to_pad).chain(st.iter()))
}

fn pad_end(st: &Str, len: usize, ch: Option<char>) -> GResult<Root<Str>> {
	let ch = ch.unwrap_or(' ');
	let to_pad = len.saturating_sub(st.len());

	glsp::str_from_iter(st.iter().chain(repeat(ch).take(to_pad)))
}

fn whitespacep(val: Val) -> GResult<bool> {
	match val {
		Val::Char(ch) => Ok(ch.is_whitespace()),
		Val::Str(st) => Ok(st.iter().all(char::is_whitespace)),
		val => bail!("expected a str or char, received {}", val.a_type_name())
	}
}

fn containsp(haystack: &Str, needle: Val) -> GResult<bool> {
	let mut needle_chars = SmallVec::<[char; 32]>::new();
	match needle {
		Val::Char(ch) => needle_chars.push(ch),
		Val::Str(st) => needle_chars.extend(st.iter()),
		val => bail!("expected a char or str, received {}", val.a_type_name())
	}

	if haystack.len() < needle_chars.len() {
		return Ok(false)
	}

	'outer: for start in 0 .. haystack.len() - needle_chars.len() {
		for i in 0 .. needle_chars.len() {
			if haystack.get::<char>(start + i).unwrap() != needle_chars[i] {
				continue 'outer
			}
		}

		return Ok(true)
	}

	Ok(false)
}

fn tab(entries: Rest<(Val, Val)>) -> GResult<Root<Tab>> {
	let tab = glsp::tab_with_capacity(entries.len());

	for &(ref key, ref val) in &entries {
		tab.set(key, val)?;
	}
	
	Ok(tab)
}

fn extend(tab: Root<Tab>, entries: Rest<(Val, Val)>) -> GResult<()> {
	ensure!(tab.can_mutate(), "attempted to mutate an immutable tab");

	for &(ref key, ref val) in &entries {
		tab.set(key, val)?;
	}
	
	Ok(())
}
