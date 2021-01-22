use super::ast::{self, Alias, Ast, Expr, Id, Node, ParamList, Range};
use super::code::{Bytecode, Instr, JumpBytes, Lambda, ParamMap, Stay, StaySource, SymBytes};
use super::engine::{glsp, Span, Sym};
use super::error::GResult;
use super::gc::{Header, Raw, Root, Slot};
use super::transform::{
    op_instr_0_args, op_instr_1_arg, op_instr_2_args, op_instr_3_args, op_instr_variadic,
};
use super::val::Val;
use smallvec::SmallVec;
use std::cmp::max;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::{repeat, FromIterator};
use std::u8;

//the borrow checker doesn't yet support e.emit(CopyRegister(e.nr(dst), e.nr(src)), span); it
//complains about simultaneous mutable borrows. so until nll gets smarter, we provide this macro
//to hack around it. emit!(e, Fizz(dst, src; sym_id), span) roughly expands to:
//  {
//      e.emit(Fizz(dst.into_u8(), src.into_u8(), sym_id), span);
//      e.notify_reg(dst, 0);
//      e.notify_reg(src, 1);
//  }
macro_rules! emit (
    ($target_expr:expr, $variant_id:ident($($field_id:ident),* $(; $($field_expr:expr),*)?),
     $span_expr:expr) => (
        #[allow(unused_mut, unused_assignments, unused_variables)]
        {
            let target = &mut *$target_expr;
            target.emit(
                Instr::$variant_id(
                    $($field_id.into_u8(), )*
                    $($($field_expr),*)?
                ),
                $span_expr
            );

            let mut counter = 0usize;
            $(
                target.notify_reg($field_id, counter);
                counter += 1;
            )*
        }
    );

    ($target_expr:expr, $variant_id:ident($($field_id:ident),* $(; $($field_expr:expr),*)?)) => (
        #[allow(unused_mut, unused_assignments, unused_variables)]
        {
            let target = &mut *$target_expr;
            target.emit_no_span(
                Instr::$variant_id(
                    $($field_id.into_u8(), )*
                    $($($field_expr),*)?
                )
            );

            let mut counter = 0usize;
            $(
                target.notify_reg($field_id, counter);
                counter += 1;
            )*
        }
    );
);

struct Encoder {
    bindings: Vec<Binding>,
    frames: Vec<Frame>,
}

#[derive(Clone)]
struct Binding {
    name: Sym,
    frame: usize,
    loc: BindingLoc,
}

#[derive(Clone)]
enum BindingLoc {
    Local(u8),
    Literal(Val),
    Stay(u8),
    Toplevel(Root<Stay>),
}

impl PartialEq for BindingLoc {
    fn eq(&self, other: &BindingLoc) -> bool {
        match (self, other) {
            (BindingLoc::Local(i), BindingLoc::Local(j)) if i == j => true,
            (BindingLoc::Literal(v0), BindingLoc::Literal(v1)) if v0 == v1 => true,
            (BindingLoc::Stay(i), BindingLoc::Stay(j)) if i == j => true,
            (BindingLoc::Toplevel(ref stay0), BindingLoc::Toplevel(ref stay1)) => {
                &**stay0 as *const Stay == &**stay1 as *const Stay
            }
            _ => false,
        }
    }
}

impl Encoder {
    fn new(toplevel_lets: &HashMap<Sym, Root<Stay>>) -> Encoder {
        let mut bindings = Vec::new();
        for (name, stay) in toplevel_lets {
            bindings.push(Binding {
                name: *name,
                frame: 0, //todo: is this correct?
                loc: BindingLoc::Toplevel(stay.clone()),
            })
        }

        Encoder {
            bindings,
            frames: vec![Frame::new()],
        }
    }

    fn bind(
        &mut self,
        src: &ast::Binding,
        maybe_stay_source: StaySource,
        span: Span,
    ) -> GResult<()> {
        ensure_at!(
            span,
            src.name.is_bindable(),
            "unable to bind {} as a local",
            src.name
        );

        let loc = if src.requires_stay {
            BindingLoc::Stay(self.frame_mut().add_stay(maybe_stay_source, span)?)
        } else {
            assert!(src.alias_of.is_none());
            BindingLoc::Local(self.frame_mut().alloc_local(Val::Nil, span)?)
        };

        self.bindings.push(Binding {
            name: src.name,
            frame: self.frames.len() - 1,
            loc,
        });

        Ok(())
    }

    fn bind_and_encode(
        &mut self,
        ast: &Ast,
        src: &ast::Binding,
        maybe_stay_source: StaySource,
        is_param: bool,
        span: Span,
    ) -> GResult<()> {
        ensure_at!(
            span,
            src.name.is_bindable(),
            "unable to bind {} as a local",
            src.name
        );

        //we can skip encoding the CopyRegister to initialize this local under a few conditions...
        //- it's initialized from a Expr::Literal, or has no initializer
        //- it's an actual local or param, not an alias or stay
        //- this local's register is "fresh" (doesn't yet have an initializer)
        //- it's not nested within a (block ...), which could use (restart-block) to "initialize"
        //  the same local twice
        //- it's not in the body of a (defer-yield ...) statement, which could likewise be
        //  evaluated multiple times

        let literal_init = match src.init {
            Some(init) => match ast[init].1 {
                Expr::Literal(ref lit) => Some(lit.clone()),
                _ => None,
            },
            None => Some(Val::Nil),
        };
        let not_alias = src.alias_of.is_none();
        let not_stay = (!src.requires_stay) || is_param;
        let is_fresh = self.frame().next_local_is_fresh();
        let no_enclosing_block = self.frame().active_blocks.len() == 0;
        let not_encoding_defer = !self.frame().encoding_defer.is_some();

        let skipping_init = literal_init.is_some()
            && not_alias
            && not_stay
            && is_fresh
            && no_enclosing_block
            && not_encoding_defer;

        //generate the BindingLoc
        let loc = if src.requires_stay {
            BindingLoc::Stay(self.frame_mut().add_stay(maybe_stay_source, span)?)
        } else {
            match src.alias_of {
                Some(Alias::Literal(ref literal_val)) => BindingLoc::Literal(literal_val.clone()),
                Some(Alias::Var(name)) => self.name_binding(name).unwrap().loc,
                _ => {
                    let init_val = if skipping_init {
                        literal_init.unwrap()
                    } else {
                        Val::Nil
                    };
                    BindingLoc::Local(self.frame_mut().alloc_local(init_val, span)?)
                }
            }
        };

        //encode the initializer node
        if !skipping_init {
            let init_node = src.init.unwrap_or(ast.nil_node());

            match loc {
                BindingLoc::Local(local_id) => {
                    encode_node(self, ast, init_node, Reg::Local(local_id))?;
                }
                BindingLoc::Stay(stay_id) => {
                    let src_reg = encode_node(self, ast, init_node, Reg::Unspecified)?;
                    emit!(self.frame_mut(), MakeStay(src_reg; stay_id), span);
                    maybe_free_scratch(self, src_reg);
                }
                BindingLoc::Literal(_) => (),
                BindingLoc::Toplevel(_) => unreachable!(),
            }
        } else {
            match loc {
                BindingLoc::Local(local_i) => {
                    assert!(local_i == self.frame().local_inits.len() as u8 - 1)
                }
                BindingLoc::Stay(_) => assert!(is_param),
                _ => unreachable!(),
            }
        }

        //we don't actually bind the variable until we've finished encoding the initialiser node
        self.bindings.push(Binding {
            name: src.name,
            frame: self.frames.len() - 1,
            loc,
        });

        Ok(())
    }

    fn unbind(&mut self, src: &ast::Binding) {
        let popped = self.bindings.pop().unwrap();
        assert!(src.name == popped.name);

        if let BindingLoc::Local(local_i) = popped.loc {
            if self.bindings.iter().all(|binding| {
                (binding.frame, binding.loc.clone()) != (popped.frame, BindingLoc::Local(local_i))
            }) {
                assert!(local_i == (self.frame().active_locals - 1) as u8);
                self.frame_mut().free_local();
            }
        }
    }

    fn bind_and_encode_params(
        &mut self,
        ast: &Ast,
        param_list: &ParamList,
        span: Span,
    ) -> GResult<()> {
        let mut arg_i = 0u8;

        //we allocate a "dummy" local register for params which are actually stored in stays
        fn maybe_alloc_dummy_local(
            frame: &mut Frame,
            ast: &Ast,
            binding: &ast::Binding,
            span: Span,
        ) -> GResult<()> {
            if binding.requires_stay {
                let literal_init = match binding.init {
                    Some(init) => match ast[init].1 {
                        Expr::Literal(ref lit) => lit.clone(),
                        _ => Val::Nil,
                    },
                    None => Val::Nil,
                };

                frame.alloc_local(literal_init, span)?;
            }

            Ok(())
        }

        for binding in param_list.basic_params {
            assert!(ast[binding].init.is_none());
            self.bind(&ast[binding], StaySource::Param(arg_i), span)?;

            maybe_alloc_dummy_local(self.frame_mut(), ast, &ast[binding], span)?;

            arg_i = arg_i.checked_add(1).unwrap();
        }

        for binding in param_list.opt_params {
            assert!(ast[binding].alias_of.is_none());

            //make sure that no Instrs are emitted by evaluating this param's initializer (because
            //it should be a quoted or self-evaluating value)
            let start_instrs = self.frame().instrs.len();
            self.bind_and_encode(ast, &ast[binding], StaySource::Param(arg_i), true, span)?;
            assert!(start_instrs == self.frame().instrs.len());

            maybe_alloc_dummy_local(self.frame_mut(), ast, &ast[binding], span)?;

            arg_i = arg_i.checked_add(1).unwrap();
        }

        match param_list.rest_param {
            Some(Some(binding)) => {
                assert!(ast[binding].init.is_none());
                self.bind(&ast[binding], StaySource::Param(arg_i), span)?;

                maybe_alloc_dummy_local(self.frame_mut(), ast, &ast[binding], span)?;
            }
            Some(None) | None => (),
        }

        Ok(())
    }

    fn unbind_params(&mut self, ast: &Ast, param_list: &ParamList) {
        let iter = param_list
            .basic_params
            .chain(param_list.opt_params.chain(param_list.rest_param.flatten()));

        for binding in iter.rev() {
            if ast[binding].requires_stay {
                self.frame_mut().free_local();
            }
            self.unbind(&ast[binding]);
        }
    }

    fn name_binding(&mut self, name: Sym) -> Option<Binding> {
        self.bindings
            .iter()
            .rev()
            .find(|binding| binding.name == name)
            .cloned()
    }

    //guarantees that the given stay is made available in the current frame (creating a chain
    //of captures, if necessary) and returns its local stay id.
    fn guarantee_stay(&mut self, frame_id: usize, remote_id: u8, span: Span) -> GResult<u8> {
        assert!(frame_id <= self.frames.len() - 1);
        if frame_id == self.frames.len() - 1 {
            return Ok(remote_id);
        }

        //cause the variable to be captured by any frames between its defining frame and the
        //access point, including the current frame.
        let mut prev_stay_id = remote_id;
        for i in (frame_id + 1)..=(self.frames.len() - 1) {
            prev_stay_id = self.frames[i].add_capture(prev_stay_id, span)?;
        }

        //return the appropriate stay index
        Ok(prev_stay_id)
    }

    //guarantees that the given toplevel stay is present in the current frame's stay sources
    fn guarantee_toplevel(&mut self, stay: &Root<Stay>) -> GResult<u8> {
        //this is O(n) but unlikely to have much performance impact
        let frame = self.frame_mut();
        for (i, stay_source) in frame.stay_sources.iter().enumerate() {
            match stay_source {
                StaySource::PreExisting(ref pre_existing) => {
                    if &**pre_existing as *const Stay == &**stay as *const Stay {
                        return Ok(i as u8);
                    }
                }
                _ => (),
            }
        }

        ensure!(
            frame.stay_sources.len() < 256,
            "frame requires more than 256 stay sources"
        );

        frame
            .stay_sources
            .push(StaySource::PreExisting(stay.to_raw()));
        return Ok((frame.stay_sources.len() - 1) as u8);
    }

    fn frame(&self) -> &Frame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap()
    }
}

struct Frame {
    instrs: Vec<Instr>,
    spans: Vec<Span>,
    scratch_next: usize,
    scratch_used: usize,
    active_locals: usize,
    local_inits: Vec<Val>,
    stay_sources: Vec<StaySource>,
    stay_captures: Vec<u8>,
    lambdas: Vec<Root<Lambda>>,
    literals: Vec<Val>,
    scratch_placeholders: Vec<Placeholder>,
    literal_placeholders: Vec<Placeholder>,
    active_blocks: Vec<BlockInfo>,
    active_defers: Vec<DeferInfo>,
    defers: Vec<usize>, //start instrs
    yields: bool,

    //while encoding a (defer) form, stores the number of active BlockInfos outside of that
    //(defer). using (finish-block) or (restart-block) to exit an outer (block) is forbidden.
    encoding_defer: Option<usize>,

    //stores the highest value seen for scratch and local allocations while encoding each (defer)
    //form. after we've finished encoding the (defer), we reserve all scratch/local registers
    //which may have been used within the (defer), to prevent register reuse (github issue #17)
    peaks: Vec<Peaks>,
}

struct BlockInfo {
    name: Sym,
    first_instr: usize,
    dst_reg: Reg,
    finish_placeholders: Vec<Placeholder>,
}

#[derive(Copy, Clone)]
struct Placeholder {
    instr: usize,
    arg_id: usize,
}

//`outer_blocks` stores the number of active BlockInfos outside of the (defer)'s scope. when using
//(finish-block) or (restart-block) to exit an outer (block), RunAndPopDefer is emitted first.
#[derive(Copy, Clone)]
enum DeferInfo {
    #[allow(dead_code)]
    Defer {
        outer_blocks: usize,
        defer_id: u8,
    },
    DeferYield {
        pause_id: u8,
        resume_id: u8,
    },
}

struct Peaks {
    scratch_peak: usize,
    local_peak: usize,
}

impl Frame {
    fn new() -> Frame {
        Frame {
            instrs: Vec::new(),
            spans: Vec::new(),
            scratch_next: 0,
            scratch_used: 0,
            active_locals: 0,
            local_inits: Vec::new(),
            stay_sources: Vec::new(),
            stay_captures: Vec::new(),
            lambdas: Vec::new(),
            literals: Vec::new(),
            scratch_placeholders: Vec::new(),
            literal_placeholders: Vec::new(),
            active_blocks: Vec::new(),
            active_defers: Vec::new(),
            defers: Vec::new(),
            yields: false,
            encoding_defer: None,
            peaks: Vec::new(),
        }
    }

    fn emit(&mut self, instr: Instr, span: Span) {
        self.instrs.push(instr);
        self.spans.push(span);
    }

    fn emit_no_span(&mut self, instr: Instr) {
        //copy the span of the previously-emitted instr, where possible
        if let Some(&most_recent_span) = self.spans.last() {
            self.emit(instr, most_recent_span)
        } else {
            self.emit(instr, Span::default())
        }
    }

    fn unemit(&mut self) {
        self.instrs.pop().unwrap();
        self.spans.pop().unwrap();

        while self.scratch_placeholders.len() > 0
            && self.scratch_placeholders.last().unwrap().instr == self.instrs.len()
        {
            self.scratch_placeholders.pop().unwrap();
        }

        while self.literal_placeholders.len() > 0
            && self.literal_placeholders.last().unwrap().instr == self.instrs.len()
        {
            self.literal_placeholders.pop().unwrap();
        }
    }

    fn instrs_emitted(&self) -> usize {
        self.instrs.len()
    }

    fn alloc_scratch(&mut self, span: Span) -> GResult<u8> {
        if self.scratch_next > 255 {
            bail_at!(
                span,
                "more than 256 scratch registers required by a single frame"
            )
        }

        self.scratch_next += 1;
        if self.scratch_used < self.scratch_next {
            self.scratch_used = self.scratch_next;
        }

        for peaks in &mut self.peaks {
            peaks.scratch_peak = max(peaks.scratch_peak, self.scratch_next);
        }

        Ok((self.scratch_next - 1) as u8)
    }

    fn free_scratch(&mut self, count: usize) {
        assert!(self.scratch_next >= count);
        self.scratch_next -= count;
    }

    fn next_scratch(&self) -> u8 {
        self.scratch_next as u8
    }

    fn alloc_local(&mut self, init_val: Val, span: Span) -> GResult<u8> {
        if self.active_locals + 1 > 256 {
            bail_at!(span, "frame requires more than 256 simultaneous locals")
        }

        self.active_locals += 1;
        if self.active_locals > self.local_inits.len() {
            self.local_inits.push(init_val);
            assert!(self.active_locals == self.local_inits.len());
        }

        for peaks in &mut self.peaks {
            peaks.local_peak = max(peaks.local_peak, self.active_locals);
        }

        Ok((self.active_locals - 1) as u8)
    }

    fn next_local_is_fresh(&self) -> bool {
        self.active_locals == self.local_inits.len()
    }

    fn free_local(&mut self) {
        self.active_locals = self.active_locals.checked_sub(1).unwrap();
    }

    //cause the closure to capture the specified stay from the parent frame. cause this frame to
    //copy the captured stay into one of its own stays using a Captured stay source. return the
    //local id for the resulting stay.
    fn add_capture(&mut self, source_id: u8, span: Span) -> GResult<u8> {
        //check whether we're already capturing this stay
        for (i, &captured_id) in self.stay_captures.iter().enumerate() {
            if captured_id == source_id {
                let local_id = self
                    .stay_sources
                    .iter()
                    .position(|stay_source| {
                        if let StaySource::Captured(j) = *stay_source {
                            i == j as usize
                        } else {
                            false
                        }
                    })
                    .unwrap();

                return Ok(local_id as u8);
            }
        }

        //we're not already capturing this stay. add a new capture, as described above.
        self.stay_captures.push(source_id);
        self.add_stay(
            StaySource::Captured((self.stay_captures.len() - 1) as u8),
            span,
        )
    }

    fn add_stay(&mut self, stay_source: StaySource, span: Span) -> GResult<u8> {
        if self.stay_sources.len() + 1 > 256 {
            bail_at!(span, "frame requires more than 256 stays");
        }

        self.stay_sources.push(stay_source);
        Ok((self.stay_sources.len() - 1) as u8)
    }

    fn alloc_literal(&mut self, val: &Val, span: Span) -> GResult<u8> {
        for (i, literal) in self.literals.iter().enumerate() {
            if val.literal_eq(literal) {
                return Ok(i as u8);
            }
        }

        if self.literals.len() > 255 {
            bail_at!(span, "stack frame requires more than 255 literals")
        }

        self.literals.push(val.clone());
        Ok((self.literals.len() - 1) as u8)
    }

    fn notify_reg(&mut self, reg: Reg, arg_id: usize) {
        match reg {
            Reg::Local(_) => (),
            Reg::Scratch(_) => self.notify_scratch_placeholder(arg_id),
            Reg::Literal(_) => self.notify_literal_placeholder(arg_id),
            Reg::Unspecified => panic!(),
            Reg::Discarded => panic!(),
        }
    }

    fn notify_scratch_placeholder(&mut self, arg_id: usize) {
        self.scratch_placeholders.push(Placeholder {
            instr: self.instrs.len() - 1,
            arg_id: arg_id,
        });
    }

    fn reify_scratch_placeholders(&mut self) {
        let to_add = self.local_inits.len() as u8;

        for placeholder in &self.scratch_placeholders {
            *self.instrs[placeholder.instr].register_mut(placeholder.arg_id) += to_add;
        }
    }

    fn notify_literal_placeholder(&mut self, arg_id: usize) {
        self.literal_placeholders.push(Placeholder {
            instr: self.instrs.len() - 1,
            arg_id: arg_id,
        });
    }

    fn reify_literal_placeholders(&mut self) {
        let to_add = self.local_inits.len() as u8 + self.scratch_used as u8;

        for placeholder in &self.literal_placeholders {
            *self.instrs[placeholder.instr].register_mut(placeholder.arg_id) += to_add;
        }
    }

    fn jump_from(&mut self, index: usize, if_span: Span) -> GResult<()> {
        let to_jump = (self.instrs.len() - index) - 1;
        let new_jump_bytes = match JumpBytes::try_from(to_jump as isize) {
            Ok(bytes) => bytes,
            Err(_) => bail_at!(if_span, "attempted to jump {} instructions", to_jump),
        };

        self.instrs[index].replace_jump_bytes(new_jump_bytes);

        Ok(())
    }

    fn enter_block(&mut self, name: Sym, dst_reg: Reg) {
        self.active_blocks.push(BlockInfo {
            name: name,
            first_instr: self.instrs.len(),
            dst_reg: dst_reg,
            finish_placeholders: Vec::new(),
        });
    }

    fn leave_block(&mut self, span: Span) -> GResult<()> {
        let block = self.active_blocks.pop().unwrap();
        for placeholder in &block.finish_placeholders {
            let instr = placeholder.instr;

            let to_jump = (self.instrs.len() - 1) - instr;
            let new_jump_bytes = match JumpBytes::try_from(to_jump as isize) {
                Ok(bytes) => bytes,
                Err(_) => bail_at!(span, "attempted to jump {} instructions", to_jump),
            };

            self.instrs[instr].replace_jump_bytes(new_jump_bytes);
        }

        Ok(())
    }

    fn offset_to_block_start(&mut self, name: Sym, restart_span: Span) -> GResult<isize> {
        for block in self.active_blocks.iter().rev() {
            if block.name == name {
                let distance = (self.instrs.len() - block.first_instr) + 1;
                return Ok(-(distance as isize));
            }
        }

        bail_at!(
            restart_span,
            "invalid block name {} passed to (restart-block)",
            name
        )
    }

    fn run_and_pop_defers(&mut self, block_name: Option<Sym>, span: Span) -> GResult<()> {
        let defer_count = if let Some(block_name) = block_name {
            let new_active_blocks = match self
                .active_blocks
                .iter()
                .rposition(|block| block.name == block_name)
            {
                Some(new_active_blocks) => new_active_blocks,
                None => bail_at!(span, "invalid block name {}", block_name),
            };

            if let Some(outer_blocks) = self.encoding_defer {
                if new_active_blocks < outer_blocks {
                    bail_at!(
                        span,
                        "attempted to break out of block {} from within a (defer)",
                        block_name
                    )
                }
            }

            let mut defer_count = 0;
            for defer in self.active_defers.iter().rev() {
                if let &DeferInfo::Defer { outer_blocks, .. } = defer {
                    if new_active_blocks < outer_blocks {
                        defer_count += 1;
                    }
                }
            }

            defer_count
        } else {
            self.active_defers
                .iter()
                .filter(|d| matches!(d, &DeferInfo::Defer { .. }))
                .count()
        };

        assert!(defer_count <= 255);
        if defer_count > 0 {
            emit!(self, RunAndPopDefers(; defer_count as u8), span);
        }

        Ok(())
    }

    fn block_dst(&self, name: Sym, span: Span) -> GResult<Reg> {
        for block in self.active_blocks.iter().rev() {
            if block.name == name {
                return Ok(block.dst_reg);
            }
        }

        bail_at!(span, "invalid block name passed to (finish-block) operator")
    }

    fn notify_finish_placeholder(&mut self, name: Sym, arg_id: usize, span: Span) -> GResult<()> {
        for block in self.active_blocks.iter_mut().rev() {
            if block.name == name {
                block.finish_placeholders.push(Placeholder {
                    instr: self.instrs.len() - 1,
                    arg_id: arg_id,
                });
                return Ok(());
            }
        }

        bail_at!(span, "invalid block name passed to (finish-block) operator")
    }

    fn finalize(&mut self, return_reg: Reg) -> GResult<Vec<Slot>> {
        let span = self.spans.last().copied().unwrap_or(Span::default());

        emit!(self, Return(return_reg), span);

        let reg_count = self.local_inits.len() + self.scratch_used + self.literals.len();
        if reg_count > 256 {
            bail_at!(span, "frame requires more than 256 registers");
        }

        self.reify_scratch_placeholders();
        self.reify_literal_placeholders();

        let mut start_regs = Vec::<Slot>::with_capacity(reg_count);
        start_regs.extend(self.local_inits.iter().map(|val| Slot::from_val(val)));
        start_regs.extend(repeat(Slot::Nil).take(self.scratch_used));
        start_regs.extend(self.literals.iter().map(|lit| Slot::from_val(lit)));

        Ok(start_regs)
    }

    fn add_lambda(&mut self, lambda: Root<Lambda>, span: Span) -> GResult<u8> {
        let lambda_id = self.lambdas.len();
        self.lambdas.push(lambda);

        ensure_at!(
            span,
            lambda_id <= u8::MAX as usize,
            "more than 255 lambda expressions in one fn"
        );
        Ok(lambda_id as u8)
    }
}

pub(crate) fn encode_fragment(
    ast: &Ast,
    fragment: Id<Node>,
    toplevel_lets: &HashMap<Sym, Root<Stay>>,
) -> GResult<Root<Bytecode>> {
    let mut enc = Encoder::new(toplevel_lets);

    let return_place = encode_node(&mut enc, ast, fragment, Reg::Unspecified)?;
    let start_regs = enc.frame_mut().finalize(return_place)?;

    //destructure the bottom-most Frame and use it to construct a Bytecode
    let Frame {
        instrs,
        spans,
        lambdas,
        stay_sources,
        local_inits,
        scratch_used,
        literals,
        defers,
        ..
    } = enc.frames.pop().unwrap();

    Ok(glsp::alloc(Bytecode {
        header: Header::new(),
        instrs,
        spans,
        start_regs,
        local_count: local_inits.len() as u8,
        scratch_count: scratch_used as u8,
        literal_count: literals.len() as u8,
        start_stays: stay_sources,
        lambdas: lambdas.iter().map(|root| Raw::from_root(root)).collect(),
        defers,
    }))
}

//the encoding protocol:
//each call to encode_node receives either the register into which the form's result MUST be
//encoded, or Reg::Unspecified. encode_node returns the register into which the form's result was
//actually encoded (which is equal to dst unless dst was Reg::Unspecified). if the caller passed
//in Reg::Unspecified, but the result is a Reg::Scratch, freeing that Reg::Scratch is the caller's
//responsibility.
//
//this enables two major optimizations:
//- unnecessary copying of locals and literals to the scratch is almost totally eliminated
//- the scratch-top register is recycled wherever possible (eg, if a function-call is to emit
//  its result to the stack-top, it will use the stack-top to temporarily store its callee)
//
//encode_node also automatically frees any scratch which is allocated for a particular node,
//except for any scratch reg which is acting as the return reg. maybe_free_scratch only needs
//to be called for regs which might be allocated before the final result form is encoded (as in
//Expr::If and Expr::Do).

#[derive(PartialEq, Clone, Copy)]
enum Reg {
    Scratch(u8),
    Local(u8),
    Literal(u8),

    //can only be used as the 'dst' argument, as described above. "the result of this expression
    //has to be stored in a register, but i don't care which register you use"
    Unspecified,

    //"the result of this expression will be ignored." unlike Unspecified, if you pass Discarded
    //to encode_node as the 'dst' argument, it may return Discarded as the result's "location".
    Discarded,
}

impl Reg {
    fn into_u8(self) -> u8 {
        match self {
            Reg::Scratch(r) => r,
            Reg::Local(r) => r,
            Reg::Literal(r) => r,
            Reg::Unspecified => panic!(),
            Reg::Discarded => panic!(),
        }
    }
}

//use this when encoding an instruction which needs a dst_reg parameter. it ensures that dst
//represents an actual register, allocating a new scratch register if necessary.
fn reify_dst(enc: &mut Encoder, dst: Reg, span: Span) -> GResult<Reg> {
    Ok(match dst {
        Reg::Scratch(_) => dst,
        Reg::Local(_) => dst,
        Reg::Literal(_) => panic!(),
        Reg::Unspecified | Reg::Discarded => Reg::Scratch(enc.frame_mut().alloc_scratch(span)?),
    })
}

//to be used when encoding Op and Call instrs. if the starting_scratch_height is lower than the
//current scratch height, recycle the lowest of those scratch registers as our destination reg,
//rather than allocating even more scratch
fn recycling_reify_dst(
    enc: &mut Encoder,
    dst: Reg,
    span: Span,
    starting_scratch_height: u8,
) -> GResult<Reg> {
    Ok(match dst {
        Reg::Scratch(_) => dst,
        Reg::Local(_) => dst,
        Reg::Literal(_) => panic!(),
        Reg::Unspecified | Reg::Discarded => {
            if enc.frame().next_scratch() > starting_scratch_height {
                Reg::Scratch(starting_scratch_height)
            } else {
                Reg::Scratch(enc.frame_mut().alloc_scratch(span)?)
            }
        }
    })
}

fn maybe_free_scratch(enc: &mut Encoder, reg: Reg) {
    if let Reg::Scratch(r) = reg {
        assert!(r == enc.frame().next_scratch() - 1);
        enc.frame_mut().free_scratch(1)
    }
}

fn encode_splay(enc: &mut Encoder, splay_bits: u32, span: Span) {
    if splay_bits != 0 {
        emit!(enc.frame_mut(), Splay(; splay_bits.to_ne_bytes()), span);
    }
}

fn encode_node(enc: &mut Encoder, ast: &Ast, node: Id<Node>, dst: Reg) -> GResult<Reg> {
    let mut starting_scratch_height = enc.frame().next_scratch();

    let node_span = ast[node].0;
    let result_reg = match ast[node].1 {
        Expr::Literal(ref literal) => {
            Reg::Literal(enc.frame_mut().alloc_literal(literal, node_span)?)
        }
        Expr::Var(name) => {
            if let Some(ref binding) = enc.name_binding(name) {
                match binding.loc {
                    BindingLoc::Local(local_id) => {
                        assert!(binding.frame == enc.frames.len() - 1);
                        Reg::Local(local_id)
                    }
                    BindingLoc::Literal(ref literal_val) => {
                        Reg::Literal(enc.frame_mut().alloc_literal(literal_val, node_span)?)
                    }
                    BindingLoc::Stay(remote_id) => {
                        let local_id = enc.guarantee_stay(binding.frame, remote_id, node_span)?;

                        let dst_reg = reify_dst(enc, dst, node_span)?;
                        emit!(enc.frame_mut(), LoadStay(dst_reg; local_id), node_span);
                        dst_reg
                    }
                    BindingLoc::Toplevel(ref stay) => {
                        let local_id = enc.guarantee_toplevel(stay)?;

                        let dst_reg = reify_dst(enc, dst, node_span)?;
                        emit!(enc.frame_mut(), LoadStay(dst_reg; local_id), node_span);
                        dst_reg
                    }
                }
            } else {
                let dst_reg = reify_dst(enc, dst, node_span)?;
                emit!(enc.frame_mut(), LoadGlobal(dst_reg; SymBytes::from(name)), node_span);
                dst_reg
            }
        }
        Expr::Call {
            callee,
            args,
            splay_bits,
        } => {
            let arg_count = args.len();

            //when `dst` is explicitly set to the topmost stack register (as when we're generating
            //an argument to a CallN or a variadic OpArr), we temporarily pop that scratch register
            //so that it can be used for callees/arguments if necessary.
            let popped_dst = if starting_scratch_height > 0
                && dst == Reg::Scratch(starting_scratch_height - 1)
            {
                enc.frame_mut().free_scratch(1);
                starting_scratch_height -= 1;
                true
            } else {
                false
            };

            let dst_reg = if arg_count == 0 {
                let callee_reg = encode_node(enc, ast, callee, Reg::Unspecified)?;
                let dst_reg = recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                emit!(enc.frame_mut(), Call0(dst_reg, callee_reg), node_span);

                dst_reg
            } else if arg_count == 1 {
                let callee_reg = encode_node(enc, ast, callee, Reg::Unspecified)?;
                let arg_reg = encode_node(enc, ast, args.id_at(0), Reg::Unspecified)?;
                let dst_reg = recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                encode_splay(enc, splay_bits, node_span);
                emit!(
                    enc.frame_mut(),
                    Call1(dst_reg, callee_reg, arg_reg),
                    node_span
                );

                dst_reg
            } else if arg_count == 2 {
                let callee_reg = encode_node(enc, ast, callee, Reg::Unspecified)?;
                let arg0_reg = encode_node(enc, ast, args.id_at(0), Reg::Unspecified)?;
                let arg1_reg = encode_node(enc, ast, args.id_at(1), Reg::Unspecified)?;
                let dst_reg = recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                encode_splay(enc, splay_bits, node_span);
                emit!(
                    enc.frame_mut(),
                    Call2(dst_reg, callee_reg, arg0_reg, arg1_reg),
                    node_span
                );

                dst_reg
            } else {
                let callee_scratch = enc.frame_mut().alloc_scratch(node_span)?;
                let callee_reg = encode_node(enc, ast, callee, Reg::Scratch(callee_scratch))?;

                for arg in args {
                    let arg_scratch = enc.frame_mut().alloc_scratch(node_span)?;
                    encode_node(enc, ast, arg, Reg::Scratch(arg_scratch))?;
                }

                let dst_reg = recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                encode_splay(enc, splay_bits, node_span);
                emit!(enc.frame_mut(), CallN(dst_reg, callee_reg; arg_count as u8), node_span);

                dst_reg
            };

            if popped_dst && enc.frame().next_scratch() == starting_scratch_height {
                enc.frame_mut().alloc_scratch(node_span)?;
                starting_scratch_height += 1;
            }

            dst_reg
        }
        Expr::Op {
            op_id,
            variadic: false,
            args,
            splay_bits,
        } => {
            assert!(splay_bits == 0);
            match args.len() {
                0 => {
                    let dst_reg = reify_dst(enc, dst, node_span)?;

                    let instr = op_instr_0_args(op_id, dst_reg.into_u8());
                    enc.frame_mut().emit(instr, node_span);
                    enc.frame_mut().notify_reg(dst_reg, 0);

                    dst_reg
                }
                1 => {
                    let arg_reg = encode_node(enc, ast, args.id_at(0), Reg::Unspecified)?;
                    let dst_reg =
                        recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                    let instr = op_instr_1_arg(op_id, dst_reg.into_u8(), arg_reg.into_u8());
                    enc.frame_mut().emit(instr, node_span);
                    enc.frame_mut().notify_reg(dst_reg, 0);
                    enc.frame_mut().notify_reg(arg_reg, 1);

                    dst_reg
                }
                2 => {
                    let arg0_reg = encode_node(enc, ast, args.id_at(0), Reg::Unspecified)?;
                    let arg1_reg = encode_node(enc, ast, args.id_at(1), Reg::Unspecified)?;
                    let dst_reg =
                        recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                    let instr = op_instr_2_args(
                        op_id,
                        dst_reg.into_u8(),
                        arg0_reg.into_u8(),
                        arg1_reg.into_u8(),
                    );
                    enc.frame_mut().emit(instr, node_span);
                    enc.frame_mut().notify_reg(dst_reg, 0);
                    enc.frame_mut().notify_reg(arg0_reg, 1);
                    enc.frame_mut().notify_reg(arg1_reg, 2);

                    dst_reg
                }
                3 => {
                    let arg0_reg = encode_node(enc, ast, args.id_at(0), Reg::Unspecified)?;
                    let arg1_reg = encode_node(enc, ast, args.id_at(1), Reg::Unspecified)?;
                    let arg2_reg = encode_node(enc, ast, args.id_at(2), Reg::Unspecified)?;
                    let dst_reg =
                        recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                    let instr = op_instr_3_args(
                        op_id,
                        dst_reg.into_u8(),
                        arg0_reg.into_u8(),
                        arg1_reg.into_u8(),
                        arg2_reg.into_u8(),
                    );
                    enc.frame_mut().emit(instr, node_span);
                    enc.frame_mut().notify_reg(dst_reg, 0);
                    enc.frame_mut().notify_reg(arg0_reg, 1);
                    enc.frame_mut().notify_reg(arg1_reg, 2);
                    enc.frame_mut().notify_reg(arg2_reg, 3);

                    dst_reg
                }
                _ => panic!(),
            }
        }
        Expr::Op {
            op_id,
            variadic: true,
            args,
            splay_bits,
        } => {
            if args.len() == 0 {
                let dst_reg = reify_dst(enc, dst, node_span)?;

                let instr = op_instr_variadic(op_id, dst_reg.into_u8(), 0, 0);
                enc.frame_mut().emit(instr, node_span);
                enc.frame_mut().notify_reg(dst_reg, 0);

                dst_reg
            } else {
                let arg0_reg = Reg::Scratch(starting_scratch_height);
                for arg in args {
                    let arg_scratch = enc.frame_mut().alloc_scratch(node_span)?;
                    encode_node(enc, ast, arg, Reg::Scratch(arg_scratch))?;
                }

                let dst_reg = recycling_reify_dst(enc, dst, node_span, starting_scratch_height)?;

                encode_splay(enc, splay_bits, node_span);

                let instr = op_instr_variadic(
                    op_id,
                    dst_reg.into_u8(),
                    arg0_reg.into_u8(),
                    args.len() as u8,
                );
                enc.frame_mut().emit(instr, node_span);
                enc.frame_mut().notify_reg(dst_reg, 0);
                enc.frame_mut().notify_reg(arg0_reg, 1);

                dst_reg
            }
        }
        Expr::TypeCheck { arg, predicate } => {
            let dst_reg = reify_dst(enc, dst, node_span)?;
            let arg_reg = encode_node(enc, ast, arg, Reg::Unspecified)?;

            emit!(enc.frame_mut(), OpPredicate(dst_reg, arg_reg; predicate), node_span);

            dst_reg
        }
        Expr::Do(body) => encode_do(enc, ast, body, dst, node_span)?,
        Expr::If {
            cond,
            then_do,
            else_do,
        } => {
            let jump_placeholder = JumpBytes::try_from(0).unwrap();

            let cond_reg = encode_node(enc, ast, cond, Reg::Unspecified)?;
            emit!(enc.frame_mut(), JumpIfFalse(cond_reg; jump_placeholder), node_span);
            maybe_free_scratch(enc, cond_reg);

            //if dst_reg is anything other than Reg::Discarded, we need to reify it so that both of
            //the branches will store their result in the same register. if it's Reg::Discarded,
            //we don't have to care where the result is stored.
            let dst_reg = match dst {
                Reg::Discarded => Reg::Discarded,
                dst => reify_dst(enc, dst, node_span)?,
            };

            //either the `then` branch or the `else` branch could emit zero instructions.

            //if the `then` branch doesn't exist, we don't emit a trailing Jump at the end of the
            //`then` branch; we backtrack to change the last instr, JumpIfFalse(_), into
            //JumpIfTrue(_); and we fill in that placeholder once `else` has been encoded.

            //if the `else` branch doesn't exist, we backtrack to erase the last emitted instr,
            //which will be Jump(0).
            let start_len = enc.frame().instrs_emitted();

            //emit the `then` form
            encode_node(enc, ast, then_do, dst_reg)?;

            let then_instrs = enc.frame().instrs_emitted() - start_len;
            if then_instrs == 0 {
                //delete JumpIfFalse, replace it with JumpIfTrue
                enc.frame_mut().unemit();
                emit!(enc.frame_mut(), JumpIfTrue(cond_reg; jump_placeholder), node_span);
            } else {
                //emit Jump, reify JumpIfFalse
                emit!(enc.frame_mut(), Jump(; jump_placeholder), node_span);
                enc.frame_mut().jump_from(start_len - 1, node_span)?;
            }

            let mid_len = enc.frame().instrs_emitted();

            //emit the `else` form
            encode_node(enc, ast, else_do, dst_reg)?;

            let else_instrs = enc.frame().instrs_emitted() - mid_len;
            if then_instrs == 0 {
                enc.frame_mut().jump_from(start_len - 1, node_span)?; //reify JumpIfTrue
            } else {
                if else_instrs == 0 {
                    enc.frame_mut().unemit(); //delete Jump
                    enc.frame_mut().jump_from(start_len - 1, node_span)?; //tweak JumpIfFalse
                } else {
                    enc.frame_mut().jump_from(mid_len - 1, node_span)?; //reify Jump
                }
            }

            dst_reg
        }
        Expr::Let { .. } => {
            //we handle `let` in encode_do instead
            bail_at!(
                node_span,
                "let is not the immediate child of block, do, fn or defer"
            );
        }
        Expr::Set { target, src_node } => {
            if let Some(binding) = enc.name_binding(target) {
                match binding.loc {
                    BindingLoc::Local(local_id) => {
                        encode_node(enc, ast, src_node, Reg::Local(local_id))?
                    }
                    BindingLoc::Literal(_) => panic!(),
                    BindingLoc::Stay(remote_id) => {
                        let local_id = enc.guarantee_stay(binding.frame, remote_id, node_span)?;

                        let src_reg = encode_node(enc, ast, src_node, Reg::Unspecified)?;
                        emit!(enc.frame_mut(), SetStay(src_reg; local_id), node_span);

                        src_reg
                    }
                    BindingLoc::Toplevel(ref stay) => {
                        let local_id = enc.guarantee_toplevel(stay)?;

                        let src_reg = encode_node(enc, ast, src_node, Reg::Unspecified)?;
                        emit!(enc.frame_mut(), SetStay(src_reg; local_id), node_span);

                        src_reg
                    }
                }
            } else {
                let src_reg = encode_node(enc, ast, src_node, Reg::Unspecified)?;
                emit!(enc.frame_mut(), SetGlobal(src_reg; SymBytes::from(target)), node_span);
                src_reg
            }
        }
        Expr::Block { name, body } => {
            let dst_reg = match dst {
                Reg::Discarded => Reg::Discarded,
                dst => reify_dst(enc, dst, node_span)?,
            };

            enc.frame_mut().enter_block(name, dst_reg);
            encode_do(enc, ast, body, dst_reg, node_span)?;
            enc.frame_mut().leave_block(node_span)?;

            dst_reg
        }
        Expr::FinishBlock {
            block_name,
            result_node,
        } => {
            //find the dst reg and encode the result node to it
            let dst_reg = enc.frame_mut().block_dst(block_name, node_span)?;
            encode_node(enc, ast, result_node, dst_reg)?;

            //if this (finish-block) takes any (defer)s out of scope, pop and run them
            enc.frame_mut()
                .run_and_pop_defers(Some(block_name), node_span)?;

            //emit a placeholder Jump instr
            let jump_placeholder = JumpBytes::try_from(0).unwrap();
            emit!(enc.frame_mut(), Jump(; jump_placeholder), node_span);
            enc.frame_mut()
                .notify_finish_placeholder(block_name, 0, node_span)?;

            //evaluates to #n
            Reg::Literal(enc.frame_mut().alloc_literal(&Val::Nil, node_span)?)
        }
        Expr::RestartBlock(block_name) => {
            //if this (restart-block) takes any (defer)s out of scope, pop and run them
            enc.frame_mut()
                .run_and_pop_defers(Some(block_name), node_span)?;

            //emit the Jump instr
            let offs = enc
                .frame_mut()
                .offset_to_block_start(block_name, node_span)?;
            let jump_bytes = match JumpBytes::try_from(offs) {
                Ok(jump_bytes) => jump_bytes,
                Err(_) => bail_at!(node_span, "attempted to jump {} instructions", offs),
            };

            emit!(enc.frame_mut(), Jump(; jump_bytes), node_span);

            //evaluates to #n
            Reg::Literal(enc.frame_mut().alloc_literal(&Val::Nil, node_span)?)
        }
        Expr::Fn {
            name,
            arg_limits,
            ref param_list,
            body,
            yields,
        } => {
            //initialize the lambda's frame
            let mut frame = Frame::new();
            frame.yields = yields;
            enc.frames.push(frame);

            if let Err(source) = enc.bind_and_encode_params(ast, param_list, node_span) {
                let msg = error_at!(node_span, "error when encoding params list");
                return Err(msg.with_source(source));
            };

            //encode the fn's body forms
            let return_reg = encode_do(enc, ast, body, Reg::Unspecified, node_span)?;
            let start_regs = enc.frame_mut().finalize(return_reg)?;

            //clean up the fn's frame
            enc.unbind_params(ast, param_list);

            //package everything into a new Lambda object
            let Frame {
                instrs,
                spans,
                stay_sources,
                stay_captures,
                lambdas,
                local_inits,
                scratch_used,
                literals,
                defers,
                ..
            } = enc.frames.pop().unwrap();

            let new_lambda = glsp::alloc(Lambda {
                header: Header::new(),
                bytecode: Raw::from_root(&glsp::alloc(Bytecode {
                    header: Header::new(),
                    instrs,
                    spans,
                    start_regs,
                    start_stays: stay_sources,
                    local_count: local_inits.len() as u8,
                    scratch_count: scratch_used as u8,
                    literal_count: literals.len() as u8,
                    lambdas: lambdas.iter().map(|root| Raw::from_root(root)).collect(),
                    defers,
                })),
                param_map: ParamMap::from_param_list(param_list, &arg_limits, node_span)?,
                captures: stay_captures,
                name,
                yields,
            });

            //emit the code to create a new closure for the just-created fn
            let lambda_id = enc.frame_mut().add_lambda(new_lambda, node_span)?;
            let dst_reg = reify_dst(enc, dst, node_span)?;
            emit!(enc.frame_mut(), MakeGFn(dst_reg; lambda_id), node_span);

            dst_reg
        }
        Expr::Return(result_node) => {
            if enc.frames.len() == 1 {
                bail_at!(node_span, "(return) encountered without an enclosing (fn)")
            }

            if enc.frame().encoding_defer.is_some() {
                bail_at!(
                    node_span,
                    "(return) may not appear within (defer) or (defer-yield)"
                )
            }

            let result_reg = encode_node(enc, ast, result_node, Reg::Unspecified)?;

            enc.frame_mut().run_and_pop_defers(None, node_span)?;

            emit!(enc.frame_mut(), Return(result_reg), node_span);

            Reg::Literal(enc.frame_mut().alloc_literal(&Val::Nil, node_span)?)
        }
        Expr::Yield(result_node) => {
            if enc.frames.len() == 1 {
                bail_at!(node_span, "(yield) encountered without an enclosing (fn)")
            }

            if enc.frame().encoding_defer.is_some() {
                bail_at!(
                    node_span,
                    "(yield) may not appear within (defer) or (defer-yield)"
                )
            }

            let dst_reg = reify_dst(enc, dst, node_span)?;
            let result_reg = encode_node(enc, ast, result_node, Reg::Unspecified)?;

            //the only effect of a DeferInfo::DeferYield is to emit RunDefer instrs before and
            //after each Yield instr.
            let frame = enc.frame_mut();
            let active_defers =
                SmallVec::<[DeferInfo; 8]>::from_iter(frame.active_defers.iter().copied());

            for defer in active_defers.iter().rev() {
                if let &DeferInfo::DeferYield { pause_id, .. } = defer {
                    emit!(frame, RunDefer(; pause_id), node_span);
                }
            }

            emit!(frame, Yield(dst_reg, result_reg), node_span);

            for defer in active_defers.iter() {
                if let &DeferInfo::DeferYield { resume_id, .. } = defer {
                    emit!(frame, RunDefer(; resume_id), node_span);
                }
            }

            dst_reg
        }
        Expr::Defer(_) => {
            //like `let`, we handle `defer` in encode_do
            bail_at!(
                node_span,
                "defer is not the immediate child of block, do, fn or defer"
            );
        }
        Expr::DeferYield { .. } => {
            //like `let`, we handle `defer-yield` in encode_do
            bail_at!(
                node_span,
                "defer-yield is not the immediate child of block, do or fn"
            );
        }
    };

    //thunk the result so that it's stored in the requested dst
    let output_reg = match (result_reg, dst) {
        (Reg::Unspecified, _) | (_, Reg::Literal(_)) => panic!(),
        (_, Reg::Unspecified) => result_reg,
        (_, Reg::Discarded) => Reg::Discarded,
        (_, _) => {
            if result_reg != dst {
                emit!(enc.frame_mut(), CopyRegister(dst, result_reg));
            }
            dst
        }
    };

    //free any scratch, other than the output_reg, which was allocated for this node
    let ending_scratch_height = enc.frame().next_scratch();
    let to_free = match output_reg {
        Reg::Scratch(r) => {
            if r == starting_scratch_height {
                ending_scratch_height - (r + 1)
            } else {
                assert!(r < starting_scratch_height);
                ending_scratch_height - starting_scratch_height
            }
        }
        _ => ending_scratch_height - starting_scratch_height,
    };
    enc.frame_mut().free_scratch(to_free as usize);

    Ok(output_reg)
}

fn encode_do(
    enc: &mut Encoder,
    ast: &Ast,
    nodes: Range<Node>,
    mut dst: Reg,
    span: Span,
) -> GResult<Reg> {
    if nodes.len() == 0 {
        return Ok(Reg::Literal(
            enc.frame_mut().alloc_literal(&Val::Nil, span)?,
        ));
    }

    let mut result = None; //this will always be overwritten in the loop
    let base_defer = enc.frame().active_defers.len();

    let mut seen_defer = false;
    let mut defer_reserved_regs = SmallVec::<[(usize, usize); 8]>::new();

    for (i, node) in nodes.enumerate() {
        let node_span = ast[node].0;
        let is_last = i == nodes.len() - 1;

        match ast[node].1 {
            Expr::Let { binding } => {
                enc.bind_and_encode(ast, &ast[binding], StaySource::Empty, false, node_span)?;

                if is_last {
                    result = Some(Reg::Literal(
                        enc.frame_mut().alloc_literal(&Val::Nil, node_span)?,
                    ))
                }
            }
            Expr::Defer(body) => {
                if !seen_defer {
                    dst = reify_dst(enc, dst, span)?;
                    seen_defer = true;
                }

                let frame = enc.frame_mut();

                //open with a PushDefer instr followed by a placeholder Jump instr
                ensure_at!(
                    node_span,
                    frame.defers.len() + 1 <= 256,
                    "frame requires more than 256 (defer) forms"
                );
                let defer_id = frame.defers.len() as u8;
                frame.active_defers.push(DeferInfo::Defer {
                    outer_blocks: frame.active_blocks.len(),
                    defer_id,
                });
                emit!(frame, PushDefer(; defer_id), node_span);

                let jump_instr = frame.instrs.len();
                let jump_placeholder = JumpBytes::try_from(0).unwrap();
                emit!(frame, Jump(; jump_placeholder), node_span);

                //encode the (defer) form's body
                drop(frame);

                let reserved_regs = encode_defer(enc, ast, body, node_span)?;
                defer_reserved_regs.push(reserved_regs);

                let frame = enc.frame_mut();

                //reify the jump
                frame.jump_from(jump_instr, node_span)?;

                //the form evaluates to #n if it's the last form in the (do)
                if is_last {
                    result = Some(Reg::Literal(frame.alloc_literal(&Val::Nil, node_span)?))
                }
            }
            Expr::DeferYield {
                pause_node,
                resume_node,
            } => {
                if !seen_defer {
                    dst = reify_dst(enc, dst, span)?;
                    seen_defer = true;
                }

                //if we're in a frame that never yields, we can skip the (defer-yield)
                //altogether. this means that the forms will never be compiled, but there
                //are relatively few errors which are caught during compilation rather than
                //during expansion or execution.
                let mut frame = enc.frame_mut();
                if frame.yields {
                    //as above, except that we use one Jump to skip both defer blocks, and
                    //we don't emit PushDefer (because there's no need to execute these
                    //defer forms when an error occurs)
                    ensure_at!(
                        node_span,
                        frame.defers.len() + 2 <= 256,
                        "frame requires more than 256 (defer) forms"
                    );
                    let pause_id = frame.defers.len() as u8;
                    let resume_id = (frame.defers.len() + 1) as u8;
                    frame.active_defers.push(DeferInfo::DeferYield {
                        pause_id,
                        resume_id,
                    });

                    //the jump placeholder
                    let jump_instr = frame.instrs.len();
                    let jump_placeholder = JumpBytes::try_from(0).unwrap();
                    emit!(frame, Jump(; jump_placeholder), node_span);

                    //encode the two nodes
                    drop(frame);

                    let reserved = encode_defer(enc, ast, Range::from_id(pause_node), node_span)?;
                    defer_reserved_regs.push(reserved);

                    let reserved = encode_defer(enc, ast, Range::from_id(resume_node), node_span)?;
                    defer_reserved_regs.push(reserved);

                    frame = enc.frame_mut();

                    //reify the jump
                    frame.jump_from(jump_instr, node_span)?;
                }

                //the form evaluates to #n if it's the last form in the (do)
                if is_last {
                    result = Some(Reg::Literal(frame.alloc_literal(&Val::Nil, node_span)?))
                }
            }
            _ => {
                if !is_last {
                    let discard_reg = encode_node(enc, ast, nodes.id_at(i), Reg::Discarded)?;
                    assert!(discard_reg == Reg::Discarded);
                } else {
                    result = Some(encode_node(enc, ast, nodes.id_at(nodes.len() - 1), dst)?)
                }
            }
        }
    }

    //undo any (let) bindings, and any scratch/local registers reserved by (defer)s
    for node in nodes.rev() {
        let to_pop = match ast[node].1 {
            Expr::Let { binding } => {
                enc.unbind(&ast[binding]);
                0
            }
            Expr::Defer(_) => 1,
            Expr::DeferYield { .. } => {
                if enc.frame_mut().yields {
                    2
                } else {
                    0
                }
            }
            _ => 0,
        };

        let frame = enc.frame_mut();
        for _ in 0..to_pop {
            let (scratch_to_pop, locals_to_pop) = defer_reserved_regs.pop().unwrap();

            for _ in 0..locals_to_pop {
                frame.free_local();
            }

            frame.free_scratch(scratch_to_pop);
        }
    }

    assert!(defer_reserved_regs.is_empty());

    //emit a RunAndPopDefers instruction, if necessary
    if base_defer != enc.frame().active_defers.len() {
        let frame = enc.frame_mut();

        let mut count = 0;
        for defer in frame.active_defers.drain(base_defer..).rev() {
            if let DeferInfo::Defer { .. } = defer {
                count += 1;
            }
        }

        assert!(count <= 255);
        if count > 0 {
            emit!(frame, RunAndPopDefers(; count as u8));
        }
    }

    Ok(result.unwrap())
}

fn encode_defer(
    enc: &mut Encoder,
    ast: &Ast,
    body: Range<Node>,
    span: Span,
) -> GResult<(usize, usize)> {
    //set up the current frame for encoding a (defer) form...
    let frame = enc.frame_mut();

    let defer_start_instr = frame.instrs.len();
    frame.defers.push(defer_start_instr);

    let prev_encoding_defer = frame.encoding_defer;
    frame.encoding_defer = Some(frame.active_blocks.len());

    let peaks = Peaks {
        scratch_peak: frame.scratch_next,
        local_peak: frame.active_locals,
    };
    frame.peaks.push(peaks);

    drop(frame);

    //encode the (defer) form, followed by an EndDefer instr
    encode_do(enc, ast, body, Reg::Discarded, span)?;

    let frame = enc.frame_mut();
    emit!(frame, EndDefer(), span);

    //undo the previous changes to the frame
    let peaks = frame.peaks.pop().unwrap();

    frame.encoding_defer = prev_encoding_defer;

    //reserve any scratch and local registers used during the (defer) form
    //which are not currently allocated, and return the number of reserved regs
    let scratch_to_reserve = peaks.scratch_peak.checked_sub(frame.scratch_next).unwrap();
    let locals_to_reserve = peaks.local_peak.checked_sub(frame.active_locals).unwrap();

    for _ in 0..scratch_to_reserve {
        frame.alloc_scratch(span)?;
    }

    for _ in 0..locals_to_reserve {
        frame.alloc_local(Val::Nil, span)?;
    }

    //finished!
    Ok((scratch_to_reserve, locals_to_reserve))
}
