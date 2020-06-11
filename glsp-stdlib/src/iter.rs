use glsp::{
	Arr, bail, Callable, DequeOps, GIter, GResult, Iterable, IterableOps,
	Num, OrNil, rfn, Root, Str, Tab, Val
};

pub fn init(_sandboxed: bool) -> GResult<()> {

	//fundamental iterator operations
	glsp::bind_rfn("iter", rfn!(iter))?;
	glsp::bind_rfn("iter-next!", rfn!(iter_next))?;
	glsp::bind_rfn("iter-next-back!", rfn!(iter_next_back))?;
	glsp::bind_rfn("iter-finished?", rfn!(iter_finishedp))?;
	glsp::bind_rfn("iter-double-ended?", rfn!(iter_double_endedp))?;

	//constructors for basic iterators
	glsp::bind_rfn("rn", rfn!(rn))?;
	glsp::bind_rfn("rni", rfn!(rni))?;
	glsp::bind_rfn("once", rfn!(once))?;
	glsp::bind_rfn("once-with", rfn!(once_with))?;
	glsp::bind_rfn("repeat", rfn!(repeat))?;
	glsp::bind_rfn("repeat-with", rfn!(repeat_with))?;
	glsp::bind_rfn("chunks", rfn!(chunks))?;
	glsp::bind_rfn("chunks-exact", rfn!(chunks_exact))?;
	glsp::bind_rfn("rchunks", rfn!(rchunks))?;
	glsp::bind_rfn("rchunks-exact", rfn!(rchunks_exact))?;
	glsp::bind_rfn("windows", rfn!(windows))?;
	glsp::bind_rfn("lines", rfn!(lines))?;
	glsp::bind_rfn("split", rfn!(split))?;
	glsp::bind_rfn("keys", rfn!(keys))?;
	glsp::bind_rfn("values", rfn!(values))?;

	//constructors for iterator adapters
	glsp::bind_rfn("rev", rfn!(rev))?;
	glsp::bind_rfn("enumerate", rfn!(enumerate))?;
	glsp::bind_rfn("cloned", rfn!(cloned))?;
	glsp::bind_rfn("deep-cloned", rfn!(deep_cloned))?;
	glsp::bind_rfn("step-by", rfn!(step_by))?;
	glsp::bind_rfn("map", rfn!(map))?;
	glsp::bind_rfn("filter", rfn!(filter))?;
	glsp::bind_rfn("zip", rfn!(zip))?;
	glsp::bind_rfn("chain", rfn!(chain))?;
	glsp::bind_rfn("flatten", rfn!(flatten))?;
	glsp::bind_rfn("cycle", rfn!(cycle))?;
	glsp::bind_rfn("take", rfn!(take))?;
	glsp::bind_rfn("take-while", rfn!(take_while))?;
	glsp::bind_rfn("skip", rfn!(skip))?;
	glsp::bind_rfn("skip-while", rfn!(skip_while))?;

	//functions which consume an iterator or iterable
	glsp::bind_rfn("count", rfn!(count))?;
	glsp::bind_rfn("nth", rfn!(nth))?;
	glsp::bind_rfn("nth-back", rfn!(nth_back))?;
	glsp::bind_rfn("any?", rfn!(anyp))?;
	glsp::bind_rfn("all?", rfn!(allp))?;
	glsp::bind_rfn("find", rfn!(find))?;
	glsp::bind_rfn("rfind", rfn!(rfind))?;
	glsp::bind_rfn("fold", rfn!(fold))?;
	glsp::bind_rfn("rfold", rfn!(rfold))?;

	Ok(())
}

fn iter(iterable: Iterable) -> Root<GIter> {
	iterable.giter()
}

fn iter_next(giter: &GIter) -> GResult<Val> {
	giter.next().unwrap_or(Ok(Val::Nil))
}

fn iter_next_back(giter: &GIter) -> GResult<Val> {
	giter.next_back().unwrap_or(Ok(Val::Nil))
}

fn iter_finishedp(giter: &GIter) -> bool {
	giter.is_finished()
}

fn iter_double_endedp(giter: &GIter) -> bool {
	giter.is_double_ended()
}

fn rn(i0: Num, i1: Option<OrNil<Num>>, step_by: Option<Num>) -> GResult<Root<GIter>> {
	let step_by = step_by.unwrap_or(Num::Int(1));
	match (i0, i1) {
		(end, None) => glsp::rn(Num::Int(0), Some(end), step_by),
		(start, Some(OrNil(end))) => glsp::rn(start, end, step_by)
	}
}

fn rni(i0: Num, i1: Option<OrNil<Num>>, step_by: Option<Num>) -> GResult<Root<GIter>> {
	let step_by = step_by.unwrap_or(Num::Int(1));
	match (i0, i1) {
		(end, None) => glsp::rni(Num::Int(0), Some(end), step_by),
		(start, Some(OrNil(end))) => glsp::rni(start, end, step_by)
	}
}

fn keys(tab: &Tab) -> Root<GIter> {
	tab.gkeys()
}

fn values(tab: &Tab) -> Root<GIter> {
	tab.gvalues()
}

fn once(args: &[Val]) -> Root<GIter> {
	glsp::once(args)
}

fn once_with(callable: Callable) -> Root<GIter> {
	glsp::once_with(callable)
}

fn repeat(args: &[Val]) -> GResult<Root<GIter>> {
	glsp::repeat(args)
}

fn repeat_with(callable: Callable) -> Root<GIter> {
	glsp::repeat_with(callable)
}

fn chunks(chunk_len: usize, arr: Root<Arr>) -> GResult<Root<GIter>> {
	glsp::chunks(chunk_len, &arr)
}

fn chunks_exact(chunk_len: usize, arr: Root<Arr>) -> GResult<Root<GIter>> {
	glsp::chunks_exact(chunk_len, &arr)
}
fn rchunks(chunk_len: usize, arr: Root<Arr>) -> GResult<Root<GIter>> {
	glsp::rchunks(chunk_len, &arr)
}

fn rchunks_exact(chunk_len: usize, arr: Root<Arr>) -> GResult<Root<GIter>> {
	glsp::rchunks_exact(chunk_len, &arr)
}

fn windows(window_len: usize, arr: Root<Arr>) -> GResult<Root<GIter>> {
	glsp::windows(window_len, &arr)
}

fn lines(st: Root<Str>) -> Root<GIter> {
	glsp::lines(&st)
}

fn split(src: Root<Str>, split_at: Val) -> GResult<Root<GIter>> {
	match split_at {
		Val::Char(ch) => {
			let split_at = glsp::str_with_capacity(1);
			split_at.push(ch).unwrap();
			Ok(glsp::split(&src, &split_at))
		}
		Val::Str(ref split_at) => Ok(glsp::split(&src, split_at)),
		val => bail!("expected a str or a char, received {}", val.a_type_name())
	}
}

fn rev(base: Iterable) -> GResult<Root<GIter>> {
	glsp::rev(&base.giter())
}

fn enumerate(base: Iterable) -> Root<GIter> {
	glsp::enumerate(&base.giter())
}

fn cloned(base: Iterable) -> Root<GIter> {
	glsp::cloned(&base.giter())
}

fn deep_cloned(base: Iterable) -> Root<GIter> {
	glsp::deep_cloned(&base.giter())
}

fn step_by(step_by: usize, base: Iterable) -> GResult<Root<GIter>> {
	glsp::step_by(step_by, &base.giter())
}

fn map(callable: Callable, base: Iterable) -> Root<GIter> {
	glsp::map(&callable, &base.giter())
}

fn filter(callable: Callable, base: Iterable) -> Root<GIter> {
	glsp::filter(&callable, &base.giter())
}

fn zip(iterables: &[Iterable]) -> Root<GIter> {
	glsp::zip(iterables)
}

fn chain(iterables: &[Iterable]) -> Root<GIter> {
	glsp::chain(iterables)
}

fn flatten(base: Iterable) -> Root<GIter> {
	glsp::flatten(&base.giter())
}

fn count(iterable: Iterable) -> usize {
	iterable.giter().count()
}

fn cycle(base: Iterable) -> Root<GIter> {
	glsp::cycle(&base.giter())
}

fn take(n: usize, base: Iterable) -> Root<GIter> {
	glsp::take(n, &base.giter())
}

fn take_while(callable: Callable, base: Iterable) -> Root<GIter> {
	glsp::take_while(&callable, &base.giter())
}

fn skip(n: usize, base: Iterable) -> Root<GIter> {
	glsp::skip(n, &base.giter())
}

fn skip_while(callable: Callable, base: Iterable) -> Root<GIter> {
	glsp::skip_while(&callable, &base.giter())
}

fn nth(n: usize, iterable: Iterable) -> GResult<Option<Val>> {
	match iterable.giter().nth(n) {
		Some(Ok(val)) => Ok(Some(val)),
		Some(Err(error)) => Err(error),
		None => Ok(None)
	}
}

fn nth_back(n: usize, iterable: Iterable) -> GResult<Option<Val>> {
	match iterable.giter().nth_back(n) {
		Some(Ok(val)) => Ok(Some(val)),
		Some(Err(error)) => Err(error),
		None => Ok(None)
	}
}

fn anyp(callable: Callable, iterable: Iterable) -> GResult<bool> {
	for result in iterable.giter() {
		let result: Val = glsp::call(&callable, &[result?])?;
		if result.is_truthy() {
			return Ok(true)
		}
	}

	Ok(false)
}

fn allp(callable: Callable, iterable: Iterable) -> GResult<bool> {
	for result in iterable.giter() {
		let result: Val = glsp::call(&callable, &[result?])?;
		if result.is_falsy() {
			return Ok(false)
		}
	}

	Ok(true)
}

fn find(callable: Callable, iterable: Iterable) -> GResult<Option<Val>> {
	for maybe_arg in iterable.giter() {
		let arg = maybe_arg?;
		let result: Val = glsp::call(&callable, &[arg.clone()])?;
		if result.is_truthy() {
			return Ok(Some(arg))
		}
	}

	Ok(None)
}

fn rfind(callable: Callable, iterable: Iterable) -> GResult<Option<Val>> {
	for maybe_arg in iterable.giter().rev() {
		let arg = maybe_arg?;
		let result: Val = glsp::call(&callable, &[arg.clone()])?;
		if result.is_truthy() {
			return Ok(Some(arg))
		}
	}

	Ok(None)
}

fn fold(callable: Callable, mut accum: Val, iterable: Iterable) -> GResult<Val> {
	for result in iterable.giter() {
		accum = glsp::call(&callable, &[accum, result?])?;
	}

	Ok(accum)
}

fn rfold(callable: Callable, mut accum: Val, iterable: Iterable) -> GResult<Val> {
	for result in iterable.giter().rev() {
		accum = glsp::call(&callable, &[accum, result?])?;
	}

	Ok(accum)
}
