use super::ast::{Alias, Ast, Binding, Expr, Id, Node, Range};
use super::code::Instr;
use super::engine::{stock_syms::*, with_known_ops, Sym};
use std::collections::HashMap;
use std::iter::FromIterator;

#[cfg(feature = "compiler")]
use serde::{Deserialize, Serialize};

//we do all of the standard passes in one actual traversal, because it's simpler, more
//cache-friendly, and there's no reason not to.
pub(crate) fn standard_passes(ast: &mut Ast, node: Id<Node>) {
    with_known_ops(|known_ops| {
        transform(ast, node, &mut |ast, node, bindings| {
            unalias_node(ast, node, bindings);
            stays_node(ast, node, bindings);
            yields_node(ast, node, bindings);
            op_calls_node(ast, node, bindings, known_ops);
        });
    })
}

//transform() performs a mutating traversal over all nodes, one callback per node. each node's
//children are only traversed after the callback has had the opportunity to mutate the node
//(this traversal order is similar to macro expansion).

//it also keeps track of which local bindings (let forms and fn parameters) are active for each
//node. they can be queried using the callback's &mut Bindings argument. each binding's
//`requires_stay` flag can be mutated by the callback, in which case those changes will be
//reflected back onto the node from which the binding originated.

//in the future, we may want to provide similar information to macros - like clojure's `&env`.

fn transform<F>(ast: &mut Ast, node: Id<Node>, f: &mut F)
where
    F: FnMut(&mut Ast, Id<Node>, &mut Bindings),
{
    let mut bindings = Bindings::new();

    recurse(ast, node, &mut bindings, f);

    assert!(bindings.bindings.len() == 0);
}

fn recurse_do<F>(ast: &mut Ast, nodes: Range<Node>, bindings: &mut Bindings, f: &mut F)
where
    F: FnMut(&mut Ast, Id<Node>, &mut Bindings),
{
    for node in nodes {
        recurse(ast, node, bindings, f);

        if let Expr::Let { binding } = ast[node].1 {
            //we presumptively mark any new `let` binding as an alias if it refers to either a
            //literal, or the simple name of another local variable.
            let alias_of = ast[binding]
                .init
                .map(|init| match ast[init].1 {
                    Expr::Var(var_name) => match bindings.lookup(var_name) {
                        Some(_) => Some(Alias::Var(var_name)),
                        _ => None,
                    },
                    Expr::Literal(ref val) => Some(Alias::Literal(val.clone())),
                    _ => None,
                })
                .flatten();

            bindings.push_binding(&ast[binding], alias_of);
        }
    }

    for node in nodes.rev() {
        if let Expr::Let { binding } = ast[node].1 {
            bindings.pop_binding(&mut ast[binding]);
        }
    }
}

fn recurse<F>(ast: &mut Ast, node: Id<Node>, bindings: &mut Bindings, f: &mut F)
where
    F: FnMut(&mut Ast, Id<Node>, &mut Bindings),
{
    f(ast, node, bindings);

    match ast[node].1 {
        Expr::Literal(_) => (),
        Expr::Var(_) => (),
        Expr::Call { callee, args, .. } => {
            recurse(ast, callee, bindings, f);
            for arg in args {
                recurse(ast, arg, bindings, f);
            }
        }
        Expr::Op { args, .. } => {
            for arg in args {
                recurse(ast, arg, bindings, f);
            }
        }
        Expr::TypeCheck { arg, .. } => {
            recurse(ast, arg, bindings, f);
        }
        Expr::Do(nodes) => {
            recurse_do(ast, nodes, bindings, f);
        }
        Expr::If {
            cond,
            then_do,
            else_do,
        } => {
            recurse(ast, cond, bindings, f);
            recurse(ast, then_do, bindings, f);
            recurse(ast, else_do, bindings, f);
        }
        Expr::Let { binding } => {
            recurse(ast, ast[binding].init.unwrap(), bindings, f);
        }
        Expr::Set { src_node, .. } => {
            recurse(ast, src_node, bindings, f);
        }
        Expr::Block { body, .. } => {
            recurse_do(ast, body, bindings, f);
        }
        Expr::FinishBlock { result_node, .. } => {
            recurse(ast, result_node, bindings, f);
        }
        Expr::RestartBlock(_) => (),
        Expr::Fn {
            body,
            ref param_list,
            ..
        } => {
            let param_list = param_list.clone();

            bindings.push_fn();
            bindings.fn_depth += 1;

            for binding in &ast[param_list.basic_params] {
                bindings.push_binding(binding, None);
                assert!(binding.init.is_none());
            }

            for binding in param_list.opt_params {
                if let Some(init) = ast[binding].init {
                    recurse(ast, init, bindings, f);
                }

                bindings.push_binding(&ast[binding], None);
            }

            if let Some(Some(binding)) = param_list.rest_param {
                bindings.push_binding(&ast[binding], None);
                debug_assert!(ast[binding].init.is_none());
            }

            recurse_do(ast, body, bindings, f);

            if let Some(Some(binding)) = param_list.rest_param {
                bindings.pop_binding(&mut ast[binding]);
            }

            for binding in param_list.opt_params.rev() {
                bindings.pop_binding(&mut ast[binding]);
            }

            for binding in param_list.basic_params.rev() {
                bindings.pop_binding(&mut ast[binding]);
            }

            match ast[node].1 {
                Expr::Fn { ref mut yields, .. } => {
                    bindings.pop_fn(yields);
                }
                _ => panic!(),
            }
            bindings.fn_depth -= 1;
        }
        Expr::Return(node) => {
            recurse(ast, node, bindings, f);
        }
        Expr::Yield(node) => {
            recurse(ast, node, bindings, f);
        }
        Expr::Defer(nodes) => {
            recurse_do(ast, nodes, bindings, f);
        }
        Expr::DeferYield {
            pause_node,
            resume_node,
        } => {
            recurse(ast, pause_node, bindings, f);
            recurse(ast, resume_node, bindings, f);
        }
    }
}

struct Bindings {
    bindings: Vec<BindingInfo>,
    fns: Vec<FnInfo>,
    fn_depth: usize,
}

struct BindingInfo {
    name: Sym,
    fn_depth: usize,
    requires_stay: bool,
    alias_of: Option<Alias>,
}

struct FnInfo {
    yields: bool,
}

impl Bindings {
    fn new() -> Bindings {
        Bindings {
            bindings: Vec::new(),
            fns: Vec::new(),
            fn_depth: 0,
        }
    }

    fn lookup(&mut self, name: Sym) -> Option<&mut BindingInfo> {
        for info in self.bindings.iter_mut().rev() {
            if info.name == name {
                return Some(info);
            }
        }

        None
    }

    fn name_has_binding(&mut self, name: Sym) -> bool {
        self.lookup(name).is_some()
    }

    fn push_binding(&mut self, binding: &Binding, alias_of: Option<Alias>) {
        self.bindings.push(BindingInfo {
            name: binding.name,
            fn_depth: self.fn_depth,
            requires_stay: binding.requires_stay,
            alias_of,
        });
    }

    fn pop_binding(&mut self, binding: &mut Binding) {
        let popped = self.bindings.pop().unwrap();
        assert!(popped.name == binding.name);

        binding.requires_stay |= popped.requires_stay;

        binding.alias_of = popped.alias_of;
    }

    fn innermost_fn(&mut self) -> Option<&mut FnInfo> {
        self.fns.last_mut()
    }

    fn push_fn(&mut self) {
        self.fns.push(FnInfo { yields: false });
    }

    fn pop_fn(&mut self, yields_flag: &mut bool) {
        let popped = self.fns.pop().unwrap();
        *yields_flag |= popped.yields;
    }
}

//the "unalias" transform pass. when a local variable is mutated, mark it as non-aliasing, and
//do the same for any bindings which are currently aliasing the mutated variable.

fn unalias_node(ast: &mut Ast, node: Id<Node>, bindings: &mut Bindings) {
    match ast[node].1 {
        Expr::Set { target: name, .. } => {
            let bindings = &mut bindings.bindings;

            let mut search_i = bindings.len();
            while search_i > 0 {
                search_i -= 1;

                if bindings[search_i].name == name {
                    bindings[search_i].alias_of = None;

                    for wipe_i in search_i + 1..bindings.len() {
                        if bindings[wipe_i].alias_of == Some(Alias::Var(name)) {
                            bindings[wipe_i].alias_of = None
                        }
                    }

                    break;
                }
            }
        }
        _ => (),
    }
}

//the "stays" transform pass. for each new binding introduced by a Expr::Let or Expr::Fn,
//determines whether it's accessed from within an enclosed Expr::Fn, in which case that binding's
//requires_stay field is set to `true`.

fn stays_node(ast: &mut Ast, node: Id<Node>, bindings: &mut Bindings) {
    match ast[node].1 {
        Expr::Var(name) | Expr::Set { target: name, .. } => {
            let fn_depth = bindings.fn_depth;
            if let Some(binding_info) = bindings.lookup(name) {
                if binding_info.fn_depth < fn_depth {
                    binding_info.requires_stay = true;
                }
            }
        }
        _ => (),
    }
}

//the "yields" transform pass. for each Expr::Yield, sets the "yields" flag on the innermost
//parent Expr::Fn to true.

fn yields_node(ast: &mut Ast, node: Id<Node>, bindings: &mut Bindings) {
    match ast[node].1 {
        Expr::Yield(..) => {
            if let Some(fn_info) = bindings.innermost_fn() {
                fn_info.yields = true;
            }
        }
        _ => (),
    }
}

//the "op_calls" transform pass
//converts CallNodes whose root is a VarNode which names a builtin function, with the correct
//number of arguments, into OpNodes.
//builtin calls whose name has been rebound to a local variable are not converted.
//splayed calls are not converted, except for (arr).

#[derive(Copy, Clone)]
pub(crate) enum KnownOp {
    Fixed(OpId, usize),
    Variadic(OpId),
    TypeCheck(Predicate),
}

#[derive(Copy, Clone)]
pub(crate) enum OpId {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Abs,
    Neg,
    Sign,
    Min,
    Max,
    Int,
    Flo,
    Bool,
    NumEq,
    Lt,
    Lte,
    Gt,
    Gte,
    Not,
    Iter,
    IterNext,
    IterNextBack,
    IterFinishedp,
    Len,
    Hasp,
    Access,
    SetAccess,
    Arr,
    CallMet,
    CallMetOpt,
    CallBaseRaw,
    Global,
    SetGlobal,
}

pub(crate) fn op_instr_0_args(op_id: OpId, _dst: u8) -> Instr {
    match op_id {
        _ => panic!(),
    }
}

pub(crate) fn op_instr_1_arg(op_id: OpId, dst: u8, arg: u8) -> Instr {
    use Instr::*;

    match op_id {
        OpId::Abs => OpAbs(dst, arg),
        OpId::Neg => OpNeg(dst, arg),
        OpId::Sub => OpNeg(dst, arg),
        OpId::Sign => OpSign(dst, arg),
        OpId::Int => OpInt(dst, arg),
        OpId::Flo => OpFlo(dst, arg),
        OpId::Bool => OpBool(dst, arg),
        OpId::Not => OpNot(dst, arg),
        OpId::Iter => OpIter(dst, arg),
        OpId::IterNext => OpIterNext(dst, arg),
        OpId::IterNextBack => OpIterNextBack(dst, arg),
        OpId::IterFinishedp => OpIterFinishedp(dst, arg),
        OpId::Len => OpLen(dst, arg),
        OpId::Global => OpGlobal(dst, arg),
        _ => panic!(),
    }
}

pub(crate) fn op_instr_2_args(op_id: OpId, dst: u8, arg0: u8, arg1: u8) -> Instr {
    use Instr::*;

    match op_id {
        OpId::Add => OpAdd(dst, arg0, arg1),
        OpId::Sub => OpSub(dst, arg0, arg1),
        OpId::Mul => OpMul(dst, arg0, arg1),
        OpId::Div => OpDiv(dst, arg0, arg1),
        OpId::Rem => OpRem(dst, arg0, arg1),
        OpId::Min => OpMin(dst, arg0, arg1),
        OpId::Max => OpMax(dst, arg0, arg1),
        OpId::NumEq => OpNumEq(dst, arg0, arg1),
        OpId::Lt => OpLt(dst, arg0, arg1),
        OpId::Lte => OpLte(dst, arg0, arg1),
        OpId::Gt => OpGt(dst, arg0, arg1),
        OpId::Gte => OpGte(dst, arg0, arg1),
        OpId::Access => OpAccess(dst, arg0, arg1),
        OpId::Hasp => OpHasp(dst, arg0, arg1),
        OpId::SetGlobal => OpSetGlobal(dst, arg0, arg1),
        _ => panic!(),
    }
}

pub(crate) fn op_instr_3_args(op_id: OpId, dst: u8, arg0: u8, arg1: u8, arg2: u8) -> Instr {
    use Instr::*;

    match op_id {
        OpId::SetAccess => OpSetAccess(dst, arg0, arg1, arg2),
        _ => panic!(),
    }
}

pub(crate) fn op_instr_variadic(op_id: OpId, dst: u8, arg0: u8, arg_count: u8) -> Instr {
    use Instr::*;

    match op_id {
        OpId::Arr => OpArr(dst, arg0, arg_count),
        OpId::CallMet => OpCallMet(dst, arg0, arg_count),
        OpId::CallMetOpt => OpCallMetOpt(dst, arg0, arg_count),
        OpId::CallBaseRaw => OpCallBaseRaw(dst, arg0, arg_count),
        _ => panic!(),
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "compiler", derive(Deserialize, Serialize))]
pub(crate) enum Predicate {
    Nil = 0,
    Num,
    Int,
    Flo,
    Nan,
    Inf,
    Bool,
    Sym,
    Deque,
    Arr,
    Str,
    Tab,
    GIter,
    Iterable,
    Obj,
    Class,
    GFn,
    Coro,
    RData,
    RFn,
    Callable,
    Expander,
}

const KNOWN_OPS: [(Sym, KnownOp); 54] = {
    use KnownOp::*;

    [
        (ADD_SYM, Fixed(OpId::Add, 2)),
        (SUB_SYM, Fixed(OpId::Sub, 2)),
        (MUL_SYM, Fixed(OpId::Mul, 2)),
        (DIV_SYM, Fixed(OpId::Div, 2)),
        (REM_SYM, Fixed(OpId::Rem, 2)),
        (ABS_SYM, Fixed(OpId::Abs, 1)),
        (SIGN_SYM, Fixed(OpId::Sign, 1)),
        (MIN_SYM, Fixed(OpId::Min, 2)),
        (MAX_SYM, Fixed(OpId::Max, 2)),
        (NILP_SYM, TypeCheck(Predicate::Nil)),
        (NUMP_SYM, TypeCheck(Predicate::Num)),
        (INTP_SYM, TypeCheck(Predicate::Int)),
        (FLOP_SYM, TypeCheck(Predicate::Flo)),
        (NANP_SYM, TypeCheck(Predicate::Nan)),
        (INFP_SYM, TypeCheck(Predicate::Inf)),
        (BOOLP_SYM, TypeCheck(Predicate::Bool)),
        (SYMP_SYM, TypeCheck(Predicate::Sym)),
        (DEQUEP_SYM, TypeCheck(Predicate::Deque)),
        (ARRP_SYM, TypeCheck(Predicate::Arr)),
        (STRP_SYM, TypeCheck(Predicate::Str)),
        (TABP_SYM, TypeCheck(Predicate::Tab)),
        (ITERP_SYM, TypeCheck(Predicate::GIter)),
        (ITERABLEP_SYM, TypeCheck(Predicate::Iterable)),
        (OBJP_SYM, TypeCheck(Predicate::Obj)),
        (CLASSP_SYM, TypeCheck(Predicate::Class)),
        (FNP_SYM, TypeCheck(Predicate::GFn)),
        (COROP_SYM, TypeCheck(Predicate::Coro)),
        (RDATAP_SYM, TypeCheck(Predicate::RData)),
        (RFNP_SYM, TypeCheck(Predicate::RFn)),
        (CALLABLEP_SYM, TypeCheck(Predicate::Callable)),
        (EXPANDERP_SYM, TypeCheck(Predicate::Expander)),
        (INT_SYM, Fixed(OpId::Int, 1)),
        (FLO_SYM, Fixed(OpId::Flo, 1)),
        (BOOL_SYM, Fixed(OpId::Bool, 1)),
        (NUM_EQ_SYM, Fixed(OpId::NumEq, 2)),
        (LT_SYM, Fixed(OpId::Lt, 2)),
        (LTE_SYM, Fixed(OpId::Lte, 2)),
        (GT_SYM, Fixed(OpId::Gt, 2)),
        (GTE_SYM, Fixed(OpId::Gte, 2)),
        (NOT_SYM, Fixed(OpId::Not, 1)),
        (ITER_SYM, Fixed(OpId::Iter, 1)),
        (ITER_NEXT_SYM, Fixed(OpId::IterNext, 1)),
        (ITER_NEXT_BACK_SYM, Fixed(OpId::IterNextBack, 1)),
        (ITER_FINISHEDP_SYM, Fixed(OpId::IterFinishedp, 1)),
        (LEN_SYM, Fixed(OpId::Len, 1)),
        (HASP_SYM, Fixed(OpId::Hasp, 2)),
        (ACCESS_SYM, Fixed(OpId::Access, 2)),
        (SET_ACCESS_SYM, Fixed(OpId::SetAccess, 3)),
        (ARR_SYM, Variadic(OpId::Arr)),
        (CALL_MET_SYM, Variadic(OpId::CallMet)),
        (CALL_MET_OPT_SYM, Variadic(OpId::CallMetOpt)),
        (CALL_BASE_RAW_SYM, Variadic(OpId::CallBaseRaw)),
        (GLOBAL_SYM, Fixed(OpId::Global, 1)),
        (SET_GLOBAL_SYM, Fixed(OpId::SetGlobal, 2)),
    ]
};

pub(crate) fn known_ops() -> HashMap<Sym, KnownOp> {
    HashMap::from_iter(KNOWN_OPS.iter().map(|&(sym, known_op)| (sym, known_op)))
}

fn op_calls_node(
    ast: &mut Ast,
    node: Id<Node>,
    bindings: &mut Bindings,
    known_ops: &HashMap<Sym, KnownOp>,
) {
    if let Expr::Call {
        callee,
        args,
        splay_bits,
    } = ast[node].1
    {
        if let Expr::Var(callee_name) = ast[callee].1 {
            if !bindings.name_has_binding(callee_name)
                && (splay_bits == 0 || callee_name == ARR_SYM)
            {
                //we special-case `-`, since it's the only case which can accept either one
                //or two arguments, and it needs to behave differently for each
                if callee_name == SUB_SYM && args.len() == 1 {
                    ast[node].1 = Expr::Op {
                        op_id: OpId::Neg,
                        variadic: false,
                        args,
                        splay_bits,
                    };
                } else {
                    match known_ops.get(&callee_name) {
                        None => (),
                        Some(&KnownOp::Fixed(op_id, arg_count)) => {
                            if args.len() == arg_count {
                                ast[node].1 = Expr::Op {
                                    op_id: op_id,
                                    variadic: false,
                                    args,
                                    splay_bits,
                                };
                            }
                        }
                        Some(&KnownOp::Variadic(op_id)) => {
                            ast[node].1 = Expr::Op {
                                op_id: op_id,
                                variadic: true,
                                args,
                                splay_bits,
                            };
                        }
                        Some(&KnownOp::TypeCheck(predicate)) => {
                            if args.len() == 1 {
                                ast[node].1 = Expr::TypeCheck {
                                    arg: args.clone().next().unwrap(),
                                    predicate,
                                };
                            }
                        }
                    }
                }
            }
        }
    }
}
