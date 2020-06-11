use smallvec::{SmallVec};
use std::iter::{DoubleEndedIterator, ExactSizeIterator, FromIterator, FusedIterator};
use std::marker::{PhantomData};
use std::num::{NonZeroU32};
use std::ops::{Index, IndexMut};
use super::collections::{Arr, DequeAccess, DequeOps};
use super::gc::{Root};
use super::engine::{Span, stock_syms::*, Sym, SymKind};
use super::error::{GResult};
use super::val::{Val};
use super::transform::{OpId, Predicate};


//-------------------------------------------------------------------------------------------------
// Ast, Arena<T>
//-------------------------------------------------------------------------------------------------

//a naive ast, based on Box and Vec, would do too much unnecessary allocation. instead, we
//implement an arena allocator, using Vecs with u32 indexes.
pub(crate) struct Ast {
	nodes: Arena<Node>,
	bindings: Arena<Binding>,
	nil_node: Id<Node>
}

impl Ast {
	pub(crate) fn new() -> Ast {
		let mut nodes = Arena::<Node>::with_capacity(32);
		let bindings = Arena::<Binding>::with_capacity(16);
		
		//we always provide a node for the '#n' literal; it's required by encoder.rs
		let nil_node = nodes.alloc(Node(Span::default(), Expr::Literal(Val::Nil)));

		Ast { nodes, bindings, nil_node }
	}

	#[allow(dead_code)]
	pub(crate) fn clear(&mut self) {
		self.nodes.clear();
		self.bindings.clear();
	}

	pub(crate) fn node_from_val(&mut self, val: &Val, span: Span) -> GResult<Id<Node>> {
		fn check_type(val: &Val, span: Span) -> GResult<()> {
			match *val {
				Val::Arr(ref arr) => {
					for val in arr.iter() {
						check_type(&val, arr.span())?;
					}

					Ok(())
				}
				Val::Tab(ref tab) => {
					for (key, value) in tab.entries().iter() {
						check_type(&key, span)?;
						check_type(&value, span)?;
					}

					Ok(())
				}
				Val::Nil | Val::Int(..) | Val::Char(..) | Val::Flo(..) |
				Val::Bool(..) | Val::Sym(..) | Val::Str(..) => {
					Ok(())
				}
				Val::GIter(..) | Val::Obj(..) | Val::Class(..) | Val::GFn(..) | 
				Val::RFn(..) | Val::Coro(..) | Val::RData(..) => {
					bail_at!(span, "{} literals cannot be evaluated", val.type_name())
				}
			}
		}

		check_type(val, span)?;

		let node = val_to_node(self, val, span)?;
		Ok(self.node(node))
	}

	pub(crate) fn node(&mut self, node: Node) -> Id<Node> {
		self.nodes.alloc(node)
	}

	pub(crate) fn nodes<T>(&mut self, iter: T) -> GResult<Range<Node>> 
	where
		T: Iterator<Item = GResult<Node>>
	{
		self.nodes.alloc_range(iter)
	}

	pub(crate) fn nil_node(&self) -> Id<Node> {
		self.nil_node
	}

	pub(crate) fn binding(&mut self, binding: Binding) -> Id<Binding> {
		self.bindings.alloc(binding)
	}

	pub(crate) fn bindings<T>(&mut self, iter: T) -> GResult<Range<Binding>> 
	where
		T: Iterator<Item = GResult<Binding>>
	{
		self.bindings.alloc_range(iter)
	}
}

impl Index<Id<Node>> for Ast {
	type Output = Node;
	fn index(&self, index: Id<Node>) -> &Node {
		&self.nodes[index]
	}
}

impl IndexMut<Id<Node>> for Ast {
	fn index_mut(&mut self, index: Id<Node>) -> &mut Node {
		&mut self.nodes[index]
	}
}

impl Index<Range<Node>> for Ast {
	type Output = [Node];
	fn index(&self, range: Range<Node>) -> &[Node] {
		&self.nodes[range]
	}
}

impl IndexMut<Range<Node>> for Ast {
	fn index_mut(&mut self, range: Range<Node>) -> &mut [Node] {
		&mut self.nodes[range]
	}
}

impl Index<Id<Binding>> for Ast {
	type Output = Binding;
	fn index(&self, index: Id<Binding>) -> &Binding {
		&self.bindings[index]
	}
}

impl IndexMut<Id<Binding>> for Ast {
	fn index_mut(&mut self, index: Id<Binding>) -> &mut Binding {
		&mut self.bindings[index]
	}
}

impl Index<Range<Binding>> for Ast {
	type Output = [Binding];
	fn index(&self, range: Range<Binding>) -> &[Binding] {
		&self.bindings[range]
	}
}

impl IndexMut<Range<Binding>> for Ast {
	fn index_mut(&mut self, range: Range<Binding>) -> &mut [Binding] {
		&mut self.bindings[range]
	}
}

struct Arena<T> {
	vec: Vec<T>
}

impl<T: Default> Arena<T> {
	#[allow(dead_code)]
	fn new() -> Self {
		Self::with_capacity(0)
	}

	fn with_capacity(cap: usize) -> Self {
		let mut vec = Vec::with_capacity(cap);

		//we need to push a dummy element to index 0, to enable the NonZeroU32
		//optimization for indexes
		vec.push(T::default());

		Arena { vec }
	}

	fn clear(&mut self) {
		self.vec.clear()
	}

	fn alloc(&mut self, t: T) -> Id<T> {
		let index = self.vec.len() as u32;
		self.vec.push(t);
		Id(NonZeroU32::new(index).unwrap(), PhantomData)
	}

	fn alloc_range(&mut self, iter: impl Iterator<Item = GResult<T>>) -> GResult<Range<T>> {
		let start_len = self.vec.len();

		let mut err: GResult<()> = Ok(());
		self.vec.extend(iter.flat_map(|result| -> Option<T> {
			match result {
				Ok(node) => Some(node),
				Err(new_err) => {
					err = Err(new_err);
					None
				}
			}
		}));

		if let Err(err) = err {
			Err(err)
		} else {
			Ok(Range(
				NonZeroU32::new(start_len as u32).unwrap(),
				(self.vec.len() - start_len) as u32,
				PhantomData
			))
		}
	}
}

#[derive(PartialEq)]
pub(crate) struct Id<T>(NonZeroU32, PhantomData<T>);

impl<T> Clone for Id<T> {
	fn clone(&self) -> Id<T> {
		Id(self.0, self.1)
	}
}

impl<T> Copy for Id<T> { }

#[derive(PartialEq)]
pub(crate) struct Range<T>(NonZeroU32, u32, PhantomData<T>);

impl<T> Clone for Range<T> {
	fn clone(&self) -> Range<T> {
		Range(self.0, self.1, self.2)
	}
}

impl<T> Copy for Range<T> { }

impl<T> Range<T> {
	pub(crate) fn len(&self) -> usize {
		self.1 as usize
	}

	pub(crate) fn from_id(id: Id<T>) -> Self {
		Range(id.0, 1, PhantomData)
	}

	pub(crate) fn id_at(&self, i: usize) -> Id<T> {
		assert!(i < self.1 as usize);
		Id(NonZeroU32::new(self.0.get() + i as u32).unwrap(), PhantomData)
	}
}

impl<T> Iterator for Range<T> {
	type Item = Id<T>;
	fn next(&mut self) -> Option<Id<T>> {
		if self.1 == 0 {
			None
		} else {
			let i = self.0;
			self.0 = NonZeroU32::new(self.0.get() + 1).unwrap();
			self.1 -= 1;
			Some(Id(i, PhantomData))
		}
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		(self.len(), Some(self.len()))
	}
}

impl<T> DoubleEndedIterator for Range<T> {
	fn next_back(&mut self) -> Option<Id<T>> {
		if self.1 == 0 {
			None
		} else {
			let i = NonZeroU32::new(self.0.get() + self.1 - 1).unwrap();
			self.1 -= 1;
			Some(Id(i, PhantomData))
		}
	}
}

impl<T> ExactSizeIterator for Range<T> { }

impl<T> FusedIterator for Range<T> { }

impl<T> Index<Id<T>> for Arena<T> {
	type Output = T;
	fn index(&self, index: Id<T>) -> &T {
		let i = index.0.get() as usize;
		&self.vec[i]
	}
}

impl<T> IndexMut<Id<T>> for Arena<T> {
	fn index_mut(&mut self, index: Id<T>) -> &mut T {
		let i = index.0.get() as usize;
		&mut self.vec[i]
	}
}

impl<T> Index<Range<T>> for Arena<T> {
	type Output = [T];
	fn index(&self, range: Range<T>) -> &[T] {
		let start = range.0.get() as usize;
		let end = start + (range.1 as usize);
		&self.vec[start..end]
	}
}

impl<T> IndexMut<Range<T>> for Arena<T> {
	fn index_mut(&mut self, range: Range<T>) -> &mut [T] {
		let start = range.0.get() as usize;
		let end = start + (range.1 as usize);
		&mut self.vec[start..end]
	}
}


//-------------------------------------------------------------------------------------------------
// Node, Expr, Binding
//-------------------------------------------------------------------------------------------------

pub(crate) struct Node(pub(crate) Span, pub(crate) Expr);

pub(crate) enum Expr {
	Literal(Val),
	Var(Sym),
	Call {
		callee: Id<Node>,
		args: Range<Node>,
		splay_bits: u32
	},
	Op {
		op_id: OpId,
		variadic: bool,
		args: Range<Node>,
		splay_bits: u32
	},
	TypeCheck {
		arg: Id<Node>,
		predicate: Predicate
	},
	Do(Range<Node>),
	If {
		cond: Id<Node>,
		then_do: Id<Node>,
		else_do: Id<Node>
	},
	Let {
		binding: Id<Binding>
	},
	Set {
		target: Sym,
		src_node: Id<Node>
	},
	Block {
		name: Sym,
		body: Range<Node>
	},
	FinishBlock {
		block_name: Sym,
		result_node: Id<Node>
	},
	RestartBlock(Sym),
	Fn {
		name: Option<Sym>,
		arg_limits: Option<(usize, Option<usize>)>,
		param_list: ParamList,
		body: Range<Node>,
		yields: bool
	},
	Return(Id<Node>),
	Yield(Id<Node>),
	Defer(Range<Node>),
	DeferYield {
		pause_node: Id<Node>,
		resume_node: Id<Node>
	}
}

impl Default for Node {
	fn default() -> Node {
		Node(Span::default(), Expr::Literal(Val::Nil))
	}
}

pub(crate) struct Binding {
	pub(crate) name: Sym,
	pub(crate) init: Option<Id<Node>>,
	pub(crate) requires_stay: bool,
	pub(crate) alias_of: Option<Alias>
}

impl Binding {
	fn new(name: Sym, init: Option<Id<Node>>) -> Binding {
		Binding {
			name,
			init,
			requires_stay: false,
			alias_of: None
		}
	}
}

impl Default for Binding {
	fn default() -> Binding {
		Binding {
			name: ERR_SYM,
			init: None,
			requires_stay: false,
			alias_of: None
		}
	}
}

//when a `let` binding makes it through transform.rs and is still marked as an alias afterwards, 
//this means that encoder.rs does not need to allocate a local for it - the encoder can replace any
//access to this variable with an access to the aliased local/literal/stay instead.
#[derive(PartialEq)]
pub(crate) enum Alias {
	Var(Sym),
	Literal(Val)
}


//-------------------------------------------------------------------------------------------------
// Node construction
//-------------------------------------------------------------------------------------------------

fn vals_to_nodes(ast: &mut Ast, vals: &[Val], span: Span) -> GResult<Range<Node>> {
	let mut vec = SmallVec::<[GResult<Node>; 8]>::with_capacity(vals.len());
	vec.extend(vals.iter().map(|val| val_to_node(ast, val, span)));
	ast.nodes(vec.into_iter())
}

fn val_to_node(ast: &mut Ast, val: &Val, span: Span) -> GResult<Node> {
	//self-evaluating aliasable values (strs, tabs, empty arrs) are cloned and frozen if they're
	//not already deep-frozen. we do something similar for quoted values below
	match *val {
		Val::Nil | Val::Int(..) | Val::Char(..) | Val::Flo(..) | Val::Bool(..) => {
			Ok(Node(span, Expr::Literal(val.clone())))
		}
		Val::Sym(sym) => {
			Ok(Node(span, Expr::Var(sym)))
		}
		Val::Arr(ref root) if root.len() == 0 => {
			let empty_arr = if root.is_frozen() {
				root.clone()
			} else {
				//using shallow_clone preserves the array's Span
				let empty_arr = root.shallow_clone(); 
				empty_arr.freeze();
				empty_arr
			};

			Ok(Node(span, Expr::Literal(Val::Arr(empty_arr))))
		}
		Val::Arr(ref arr) => {
			arr_to_node(ast, arr)
		}
		Val::Str(ref root) => {
			let st = if root.is_frozen() {
				root.clone()
			} else {
				let st = root.shallow_clone();
				st.freeze();
				st
			};

			Ok(Node(span, Expr::Literal(Val::Str(st))))
		}
		Val::Tab(ref root) => {
			let tab = if val.is_deep_frozen() {
				root.clone()
			} else {
				let tab = root.deep_clone()?;
				tab.deep_freeze();
				tab
			};

			Ok(Node(span, Expr::Literal(Val::Tab(tab))))
		}
		Val::GIter(_) | Val::RFn(_) | Val::Obj(_) | Val::Class(_) | 
		Val::GFn(_) | Val::Coro(_) | Val::RData(_) => {
			//already checked above, in Ast::node_from_val
			unreachable!()
		}
	}
}

fn arr_to_node(ast: &mut Ast, arr: &Root<Arr>) -> GResult<Node> {
	let root = arr.get::<Val>(0)?;
	let mut args = SmallVec::<[Val; 8]>::from_iter(arr.iter().skip(1));
	let span = arr.span();
	
	match root {
		Val::Sym(sym) if sym.kind() == SymKind::StockSpecial => {
			special_to_node(ast, sym, &args, span)
		}

		//(.name ...) -> (call-meth 'name ...)
		Val::Arr(root) 
			if root.len() >= 1 && 
			   root.get::<Val>(0).unwrap() == Val::Sym(METH_NAME_SYM) => 
		{
			ensure_at!(span, root.len() == 2, "invalid (meth-name ...) abbreviation");

			let meth_name = match root.get::<Val>(1).unwrap() {
				Val::Sym(meth_name) => meth_name,
				val => bail_at!(span, "invalid (meth-name ...) form: argument is {}", 
				                val.a_type_name())
			};

			let replacement = arr.shallow_clone();
			let _: Val = replacement.pop_start()?;
			replacement.push_start(arr![QUOTE_SYM, meth_name])?;
			replacement.push_start(CALL_METH_SYM)?;

			arr_to_node(ast, &replacement)
		}

		//((? .name) ...) -> (call-meth-opt 'name ...)
		Val::Arr(root) 
			if root.len() >= 1 && 
			   root.get::<Val>(0).unwrap() == Val::Sym(QUESTION_MARK_SYM) => 
		{
			ensure_at!(span, root.len() == 2, "invalid (? ...) callee");

			let meth_name_arr = match root.get::<Val>(1).unwrap() {
				Val::Arr(arr)
					if arr.len() >= 1 &&
					   arr.get::<Val>(0).unwrap() == Val::Sym(METH_NAME_SYM) => arr,
				Val::Sym(name) => bail_at!(span, "(? {}) is not a valid callee. \
				                         did you mean (? .{})?", name, name),
				form => bail_at!(span, "(? {}) is not a valid callee", form)
			};

			ensure_at!(span, meth_name_arr.len() == 2, "invalid (meth-name ...) form");

			let meth_name = match meth_name_arr.get::<Val>(1).unwrap() {
				Val::Sym(meth_name) => meth_name,
				val => bail_at!(span, "invalid (meth-name ...) form: argument is {}", 
				                val.a_type_name())
			};

			let replacement = arr.shallow_clone();
			let _: Val = replacement.pop_start()?;
			replacement.push_start(arr![QUOTE_SYM, meth_name])?;
			replacement.push_start(CALL_METH_OPT_SYM)?;

			arr_to_node(ast, &replacement)
		}
		
		_ => {
			//detect and process (splay x) arguments
			let mut splay_bits = 0;
			for (i, arg) in args.iter_mut().enumerate() {
				if let Val::Arr(arr) = arg.clone() {
					if arr.len() == 2 && arr.get::<Val>(0)? == Val::Sym(SPLAY_SYM) {
						if i >= 32 {
							bail_at!(span, "arguments beyond the 32nd cannot be splayed")
						}

						splay_bits |= 1 << i;

						//replace the (splay arg) form with just `arg` in any subsequent processing
						*arg = arr.get(1)?;
					}
				}
			}

			let callee_node = val_to_node(ast, &root, span)?;

			Ok(Node(span, Expr::Call {
				callee: ast.node(callee_node), 
				args: vals_to_nodes(ast, &args, span)?,
				splay_bits
			}))
		}
	}
}

type SpecialInfo = (
	Sym, 
	fn(&mut Ast, &[Val], Span) -> GResult<Node>, 
	usize, 
	Option<usize>
);

const SPECIAL_COUNT: usize = 13;
static SPECIAL_INFO: [SpecialInfo; SPECIAL_COUNT] = [
	(ASSIGNMENT_SYM, set_to_node, 2, Some(2)),
	(DO_SYM, do_to_node, 0, None),
	(IF_SYM, if_to_node, 3, Some(3)),
	(LET_SYM, let_to_node, 2, Some(2)),
	(BLOCK_SYM, block_to_node, 1, None),
	(FINISH_BLOCK_SYM, finish_block_to_node, 1, Some(2)),
	(RESTART_BLOCK_SYM, restart_block_to_node, 1, Some(1)),
	(FN_SYM, fn_to_node, 1, None),
	(RETURN_SYM, return_to_node, 0, Some(1)),
	(YIELD_SYM, yield_to_node, 0, Some(1)),
	(QUOTE_SYM, quote_to_node, 1, Some(1)),
	(DEFER_SYM, defer_to_node, 0, None),
	(DEFER_YIELD_SYM, defer_yield_to_node, 2, Some(2))
];

fn special_to_node(ast: &mut Ast, name: Sym, args: &[Val], span: Span) -> GResult<Node> {
	let desc = SPECIAL_INFO.iter().find(|&desc| name == desc.0);
	let (_, func, min_args, max_args) = *desc.unwrap();

	if args.len() < min_args {
		bail_at!(span, "the {} special form expects at least {} argument{}, but only {} were \
		         provided", name, min_args, if min_args == 1 { "" } else { "s" }, args.len())
	}

	if let Some(max_args) = max_args {
		if args.len() > max_args {
			bail_at!(span, "the {} special form expects no more than {} argument{}, but {} were \
			         provided", name, max_args, if max_args == 1 { "" } else { "s" }, args.len())
		}
	}

	func(ast, args, span)
}

fn set_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	match args[0] {
		Val::Sym(sym) => {
			let src_node = val_to_node(ast, &args[1], span)?;
			Ok(Node(span, Expr::Set {
				target: sym,
				src_node: ast.node(src_node)
			}))
		}
		_ => bail_at!(span, "the first argument to (=) must be a symbol")
	}
}

fn do_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	Ok(Node(span, Expr::Do(vals_to_nodes(ast, args, span)?)))
}

fn if_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let cond_node = val_to_node(ast, &args[0], span)?;
	let then_node = val_to_node(ast, &args[1], span)?;
	let else_node = val_to_node(ast, &args[2], span)?;

	Ok(Node(span, Expr::If {
		cond: ast.node(cond_node),
		then_do: ast.node(then_node),
		else_do: ast.node(else_node)
	}))
}

fn let_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let name = match args[0] {
		Val::Sym(name) => name,
		_ => bail_at!(span, "the first argument to `let` must be a sym")
	};

	let init_node = val_to_node(ast, &args[1], span)?;
	let binding = Binding::new(name, Some(ast.node(init_node)));

	Ok(Node(span, Expr::Let {
		binding: ast.binding(binding)
	}))
}

fn block_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let name = match args[0] {
		Val::Sym(sym) => sym,
		_ => bail_at!(span, "the first argument to block must be a symbol literal")
	};
	
	Ok(Node(span, Expr::Block {
		name,
		body: vals_to_nodes(ast, &args[1..], span)?
	}))
}

fn finish_block_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let block_name = match args[0] {
		Val::Sym(sym) => sym,
		_ => bail_at!(span, "the first argument to (finish-block) must be a symbol literal")
	};
	
	let result_node = if args.len() == 1 {
		val_to_node(ast, &Val::Nil, span)?
	} else {
		val_to_node(ast, &args[1], span)?
	};
	
	Ok(Node(span, Expr::FinishBlock {
		block_name,
		result_node: ast.node(result_node)
	}))
}

fn restart_block_to_node(_ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let block_name = match args[0] {
		Val::Sym(sym) => sym,
		_ => bail_at!(span, "the first argument to (restart-block) must be a symbol literal")
	};
	
	Ok(Node(span, Expr::RestartBlock(block_name)))
}

fn fn_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let mut name = None;
	let mut arg_limits = None;

	let mut i = 0;
	let param_list = loop {
		if i >= args.len() {
			bail_at!(span, "(fn) form has no parameter list")
		}

		match args[i] {
			Val::Arr(ref arr) => break ParamList::from_arr(ast, arr)?,
			Val::Sym(FLAG_NAME_SYM) => {
				ensure_at!(span, args.len() >= i + 1, "invalid &name flag in (fn)");
				ensure_at!(span, name.is_none(), "duplicate &name flag in (fn)");

				name = match args[i + 1] {
					Val::Sym(name) => Some(name),
					ref arg => bail_at!(span, "{} is not a valid fn name", arg)
				};

				i += 2;
			}
			Val::Sym(FLAG_ARG_LIMITS_SYM) => {
				ensure_at!(span, args.len() >= i + 2, "invalid &arg-limits flag in (fn)");
				ensure_at!(span, arg_limits.is_none(), "duplicate &arg-limits flag in (fn)");
				
				let min_args = match args[i + 1] {
					Val::Int(i) if i >= 0 => i as usize,
					ref arg => bail_at!(span, "{} is not a valid minimum argument count", arg)
				};

				let max_args = match args[i + 2] {
					Val::Int(i) if i >= 0 => {
						ensure_at!(span, i as usize >= min_args, "minimum arg count {} is \
						           greater than maximum arg count {}", min_args, i);
						Some(i as usize)
					}
					Val::Nil => None,
					ref arg => bail_at!(span, "{} is not a valid maximum argument count", arg)
				};

				arg_limits = Some((min_args, max_args));

				i += 3;
			}
			ref arg => bail_at!(span, "(fn) form received unexpected argument {}", arg)
		}
	};

	Ok(Node(span, Expr::Fn {
		name,
		arg_limits,
		param_list,
		body: vals_to_nodes(ast, &args[i + 1..], span)?,
		yields: false
	}))
}

fn return_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let result = if args.is_empty() {
		val_to_node(ast, &Val::Nil, span)?
	} else {
		val_to_node(ast, &args[0], span)?
	};
	
	Ok(Node(span, Expr::Return(ast.node(result))))
}

fn yield_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let result = if args.is_empty() {
		val_to_node(ast, &Val::Nil, span)?
	} else {
		val_to_node(ast, &args[0], span)?
	};
	
	Ok(Node(span, Expr::Yield(ast.node(result))))
}

fn quote_to_node(_ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let val = if args[0].is_deep_frozen() {
		args[0].clone()
	} else {
		let val = args[0].deep_clone()?;
		val.deep_freeze();
		val
	};

	Ok(Node(span, Expr::Literal(val)))
}

fn defer_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	Ok(Node(span, Expr::Defer(vals_to_nodes(ast, args, span)?)))
}

fn defer_yield_to_node(ast: &mut Ast, args: &[Val], span: Span) -> GResult<Node> {
	let pause_node = val_to_node(ast, &args[0], span)?;
	let resume_node = val_to_node(ast, &args[1], span)?;

	Ok(Node(span, Expr::DeferYield {
		pause_node: ast.node(pause_node),
		resume_node: ast.node(resume_node)
	}))
}


//-------------------------------------------------------------------------------------------------
// ParamList
//-------------------------------------------------------------------------------------------------

//terminology: a "parameter" is a variable binding in a function signature. an "argument" is
//an expression or value passed to a function invocation.

#[derive(Clone)]
pub(crate) struct ParamList {
	pub(crate) basic_params: Range<Binding>,
	pub(crate) opt_params: Range<Binding>,

	//None is an absent rest param, Some(None) is .._, Some(Some(r)) is ..name
	pub(crate) rest_param: Option<Option<Id<Binding>>>
}

impl ParamList {
	pub(crate) fn from_arr(ast: &mut Ast, arr: &Root<Arr>) -> GResult<ParamList> {
		let span = arr.span();

		//the (fn) special form has much simpler parameter lists than the (fn) macro. the
		//only allowed parameters are name, (? name), (? name trivial-initializer), .._, and 
		//..name. the ordering must be all normal params, then all (?) params, then the .. param.
		//name is a non-underscore sym and trivial-initializer is a literal. 

		let mut basic_params: SmallVec<[Binding; 8]> = SmallVec::new();
		let mut opt_params: SmallVec<[Binding; 8]> = SmallVec::new();
		let mut rest_param: Option<Option<Binding>> = None;

		for param in arr.iter() {
			match param {
				Val::Sym(name) => {
					ensure_at!(span, name != UNDERSCORE_SYM, "unexpected _ in (fn) special form");
					ensure_at!(span, rest_param.is_none() && opt_params.len() == 0, 
					           "malpositioned basic parameter passed to (fn) special form");

					basic_params.push(Binding::new(name, None))
				}
				Val::Arr(param_arr) if param_arr.len() > 0 => {
					match param_arr.get(0)? {
						Val::Sym(QUESTION_MARK_SYM) => {
							ensure_at!(span, (param_arr.len() == 2 || param_arr.len() == 3) && 
							           param_arr.get::<Val>(1)?.is_sym() && 
							           param_arr.get::<Val>(1)? != Val::Sym(UNDERSCORE_SYM), 
							           "invalid param {} in (fn) special form", &param_arr);
							ensure_at!(span, rest_param.is_none(), "(?) param after .. param");

							let name: Sym = param_arr.get(1)?;

							let init = if param_arr.len() == 2 {
								None
							} else {
								Some(ast.node_from_val(&param_arr.get(2)?, param_arr.span())?)
							};

							opt_params.push(Binding::new(name, init));
						}
						Val::Sym(SPLAY_SYM) => {
							ensure_at!(span, param_arr.len() == 2 && 
							           param_arr.get::<Val>(1)?.is_sym(), 
							           "invalid param {} in (fn) special form", &param_arr);
							ensure_at!(span, rest_param.is_none(), "duplicate .. param");

							let name: Sym = param_arr.get(1)?;

							if name == UNDERSCORE_SYM {
								rest_param = Some(None);
							} else {
								rest_param = Some(Some(Binding::new(name, None)));
							}
						}
						_ => {
							bail_at!(span, "unexpected parameter {} in (fn) special form", 
							         param_arr)
						}
					}
				}
				param => bail_at!(span, "unexpected parameter {} in (fn) special form", param)
			}
		}

		let result = ParamList {
			basic_params: ast.bindings(basic_params.into_iter().map(GResult::Ok))?,
			opt_params: ast.bindings(opt_params.into_iter().map(GResult::Ok))?,
			rest_param: match rest_param {
				Some(Some(rest_param)) => Some(Some(ast.binding(rest_param))),
				Some(None) => Some(None),
				None => None
			}
		};
		
		//return
		Ok(result)
	}
	
	pub(crate) fn param_count(&self) -> usize {
		self.basic_params.len() 
		+ self.opt_params.len()
		+ match self.rest_param {
			Some(Some(_)) => 1,
			_ => 0
		}
	}
}
