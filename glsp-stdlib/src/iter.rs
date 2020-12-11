use glsp::{
	Arr, bail, Callable, DequeOps, GIter, GResult, Iterable, IterableOps,
	Num, Rest, Root, Str, Tab, Val
};

pub fn init(_sandboxed: bool) -> GResult<()> {

	//fundamental iterator operations
	glsp::bind_rfn("iter", &iter)?;
	glsp::bind_rfn("iter-next!", &iter_next)?;
	glsp::bind_rfn("iter-next-back!", &iter_next_back)?;
	glsp::bind_rfn("iter-finished?", &iter_finishedp)?;
	glsp::bind_rfn("iter-double-ended?", &iter_double_endedp)?;

	//constructors for basic iterators
	glsp::bind_rfn("rn", &rn)?;
	glsp::bind_rfn("rni", &rni)?;
	glsp::bind_rfn("once", &once)?;
	glsp::bind_rfn("once-with", &once_with)?;
	glsp::bind_rfn("repeat", &repeat)?;
	glsp::bind_rfn("repeat-with", &repeat_with)?;
	glsp::bind_rfn("chunks", &chunks)?;
	glsp::bind_rfn("chunks-exact", &chunks_exact)?;
	glsp::bind_rfn("rchunks", &rchunks)?;
	glsp::bind_rfn("rchunks-exact", &rchunks_exact)?;
	glsp::bind_rfn("windows", &windows)?;
	glsp::bind_rfn("lines", &lines)?;
	glsp::bind_rfn("split", &split)?;
	glsp::bind_rfn("keys", &keys)?;
	glsp::bind_rfn("values", &values)?;

	//constructors for iterator adapters
	glsp::bind_rfn("rev", &rev)?;
	glsp::bind_rfn("enumerate", &enumerate)?;
	glsp::bind_rfn("cloned", &cloned)?;
	glsp::bind_rfn("deep-cloned", &deep_cloned)?;
	glsp::bind_rfn("step-by", &step_by)?;
	glsp::bind_rfn("map", &map)?;
	glsp::bind_rfn("filter", &filter)?;
	glsp::bind_rfn("zip", &zip)?;
	glsp::bind_rfn("chain", &chain)?;
	glsp::bind_rfn("flatten", &flatten)?;
	glsp::bind_rfn("cycle", &cycle)?;
	glsp::bind_rfn("take", &take)?;
	glsp::bind_rfn("take-while", &take_while)?;
	glsp::bind_rfn("skip", &skip)?;
	glsp::bind_rfn("skip-while", &skip_while)?;

	//functions which consume an iterator or iterable
	glsp::bind_rfn("count", &count)?;
	glsp::bind_rfn("nth", &nth)?;
	glsp::bind_rfn("nth-back", &nth_back)?;
	glsp::bind_rfn("any?", &anyp)?;
	glsp::bind_rfn("all?", &allp)?;
	glsp::bind_rfn("find", &find)?;
	glsp::bind_rfn("rfind", &rfind)?;
	glsp::bind_rfn("fold", &fold)?;
	glsp::bind_rfn("rfold", &rfold)?;

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

fn rn(n: Num, rest: Rest<Val>) -> GResult<Root<GIter>> {
	rn_impl(n, &rest, glsp::rn)
}

fn rni(n: Num, rest: Rest<Val>) -> GResult<Root<GIter>> {
	rn_impl(n, &rest, glsp::rni)
}

fn rn_impl(
	n: Num,
	rest: &[Val],
	constructor: fn(Num, Option<Num>, Num) -> GResult<Root<GIter>>
) -> GResult<Root<GIter>> {

	match rest.len() {
		0 => glsp::rn(Num::Int(0), Some(n), Num::Int(1)),
		1 | 2 => {
			let step_by = match rest.get(1) {
				None => Num::Int(1),
				Some(&Val::Int(i)) => Num::Int(i),
				Some(&Val::Flo(f)) => Num::Flo(f),
				Some(val) => bail!("expected a Num, received {}", val.a_type_name())
			};

			match &rest[0] {
				&Val::Nil => constructor(n, None, step_by),
				&Val::Int(i) => constructor(n, Some(Num::Int(i)), step_by),
				&Val::Flo(f) => constructor(n, Some(Num::Flo(f)), step_by),
				val => bail!("expected a Num or nil, received {}", val.a_type_name())
			}
		}
		rest_len => {
			bail!("too many arguments: received {}, but expected no more than 3", rest_len)
		}
	}
}

fn keys(tab: &Tab) -> Root<GIter> {
	tab.gkeys()
}

fn values(tab: &Tab) -> Root<GIter> {
	tab.gvalues()
}

fn once(args: Rest<Val>) -> Root<GIter> {
	glsp::once(&args)
}

fn once_with(callable: Callable) -> Root<GIter> {
	glsp::once_with(callable)
}

fn repeat(args: Rest<Val>) -> GResult<Root<GIter>> {
	glsp::repeat(&args)
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

fn zip(iterables: Rest<Iterable>) -> Root<GIter> {
	glsp::zip(&iterables)
}

fn chain(iterables: Rest<Iterable>) -> Root<GIter> {
	glsp::chain(&iterables)
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
