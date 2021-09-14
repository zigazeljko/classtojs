use relooper::{BranchMode, LoopBlock, MultipleBlock, ShapedBlock, SimpleBlock};
use std::collections::{BTreeMap, HashMap};
use std::mem;
use swc_ecma_ast as es;
use swc_ecma_utils::quote_str;
use ustr::Ustr;

use crate::codegen::mangle;
use crate::codegen::util;
use crate::ir::function::{ArithOp, BitOp, BlockId, ConvOp, Function, Instruction, NumType, Pred, Push, ShiftOp, Terminator, Value};
use crate::ir::module::NameAndType;
use crate::ir::natives::Native;
use crate::parser::types;

type PhiMap = HashMap<(BlockId, BlockId), Vec<(u32, Value)>>;
type UseMap = HashMap<u32, bool>;
type VarMap = HashMap<u32, u32>;
type Stmts = Vec<es::Stmt>;

type Loop = LoopBlock<BlockId>;
type Multiple = MultipleBlock<BlockId>;
type Shaped = ShapedBlock<BlockId>;
type Simple = SimpleBlock<BlockId>;

#[derive(Default)]
struct IdGen {
    ident: BTreeMap<u32, es::Ident>,
    next: usize,
}

struct Codegen {
    uses: UseMap,
    phis: PhiMap,
    vars: VarMap,
    labels: IdGen,
    locals: IdGen,
    hasawait: bool,
}

struct BlockCodegen<'a> {
    codegen: &'a mut Codegen,
    body: Stmts,
    stack: Vec<(u32, es::Expr)>,
}

fn gen_itrunc(expr: es::Expr) -> es::Expr {
    util::bin(es::BinaryOp::BitOr, expr, (0.0).into())
}

fn gen_ltrunc(expr: es::Expr) -> es::Expr {
    util::call(
        util::imember(util::ident("BigInt".into()).into(), util::ident("asIntN".into())),
        vec![(64.0).into(), expr],
    )
}

fn gen_ultrunc(expr: es::Expr) -> es::Expr {
    util::call(
        util::imember(util::ident("BigInt".into()).into(), util::ident("asUintN".into())),
        vec![(64.0).into(), expr],
    )
}

fn gen_conv_number(expr: es::Expr) -> es::Expr {
    util::call(util::ident("Number".into()).into(), vec![expr])
}

fn gen_conv_long(expr: es::Expr) -> es::Expr {
    util::call(util::ident("BigInt".into()).into(), vec![expr])
}

fn gen_trunc(expr: es::Expr, typ: NumType) -> es::Expr {
    match typ {
        NumType::Int => gen_itrunc(expr),
        NumType::Long => gen_ltrunc(expr),
        _ => expr,
    }
}

fn is_nonlocal_flow(inner: &[es::Stmt]) -> bool {
    if let Some((last, _)) = inner.split_last() {
        last.is_break_stmt() || last.is_continue_stmt() || last.is_return_stmt() || last.is_throw()
    } else {
        false
    }
}

fn invert(mut expr: es::Expr) -> es::Expr {
    if let es::Expr::Bin(ref mut bin) = expr {
        if bin.op == es::BinaryOp::EqEq {
            bin.op = es::BinaryOp::NotEq;
            return expr;
        }
        if bin.op == es::BinaryOp::NotEq {
            bin.op = es::BinaryOp::EqEq;
            return expr;
        }
    }
    util::unary(es::UnaryOp::Bang, expr)
}

fn gen_if(out: &mut Stmts, test: es::Expr, cons: Stmts, alt: Stmts) {
    if alt.is_empty() || is_nonlocal_flow(&cons) {
        out.push(util::cond(test, util::block(cons), None));
        out.extend(alt);
        return;
    }
    if cons.is_empty() || is_nonlocal_flow(&alt) {
        out.push(util::cond(invert(test), util::block(alt), None));
        out.extend(cons);
        return;
    }
    out.push(util::cond(test, util::block(cons), Some(util::block(alt))));
}

fn gen_value(value: Value, locals: &mut IdGen, vars: &VarMap) -> es::Expr {
    match value {
        Value::Undef => util::ident("undefined".into()).into(),
        Value::Null => es::Lit::Null(util::NULL).into(),
        Value::Integer(val) => util::number(val as f64),
        Value::Long(val) => util::long(val).into(),
        Value::Float(val) => util::ident(f32::from_bits(val).to_string()).into(),
        Value::Double(val) => util::number(f64::from_bits(val)),
        Value::Class(val) => mangle::class_e(val),
        Value::String(val) => quote_str!(val.to_string()).into(),
        Value::SSARef(iid) => locals.get(vars.get(&iid).copied().unwrap_or(iid)).into(),
    }
}

impl IdGen {
    fn get(&mut self, id: u32) -> es::Ident {
        match self.ident.get(&id) {
            Some(ident) => ident.clone(),
            None => {
                let ident = mangle::ident(&mut self.next);
                self.ident.insert(id, ident.clone());
                ident
            }
        }
    }
}

impl Codegen {
    fn new(uses: UseMap, phis: PhiMap, vars: VarMap) -> Codegen {
        Codegen {
            uses,
            phis,
            vars,
            labels: IdGen::default(),
            locals: IdGen::default(),
            hasawait: false,
        }
    }

    fn label(&mut self, lid: u16) -> es::Ident {
        self.labels.get(lid as u32)
    }

    fn ident(&mut self, iid: u32) -> es::Ident {
        self.locals.get(iid)
    }

    fn value(&mut self, value: Value) -> es::Expr {
        gen_value(value, &mut self.locals, &self.vars)
    }

    fn branchmode(&mut self, body: &mut Stmts, branch: BranchMode, next: BlockId) {
        match branch {
            BranchMode::LoopBreak(lid) => {
                let label = self.label(lid);
                body.push(util::lbreak(Some(label)));
            }
            BranchMode::LoopBreakIntoMulti(lid) => {
                let label = self.label(lid);
                self.setlabel(body, next);
                body.push(util::lbreak(Some(label)));
            }
            BranchMode::LoopContinue(lid) => {
                let label = self.label(lid);
                body.push(util::lcontinue(Some(label)));
            }
            BranchMode::LoopContinueIntoMulti(lid) => {
                let label = self.label(lid);
                self.setlabel(body, next);
                body.push(util::lcontinue(Some(label)));
            }
            BranchMode::MergedBranch => {}
            BranchMode::MergedBranchIntoMulti => {
                self.setlabel(body, next);
            }
            BranchMode::SetLabelAndBreak => {
                self.setlabel(body, next);
                body.push(util::lbreak(None));
            }
        }
    }

    fn setlabel(&mut self, body: &mut Stmts, to: BlockId) {
        let assign = util::passign(es::AssignOp::Assign, self.ident(!0), f64::from(to.0).into());
        body.push(util::wrap(assign));
    }

    fn movephis(&mut self, stmts: &mut Vec<es::Stmt>, from: BlockId, to: BlockId) {
        if let Some(phis) = self.phis.get(&(from, to)) {
            for (id, value) in phis {
                if let Value::SSARef(iid) = value {
                    if self.vars.get(iid) == Some(id) {
                        continue;
                    }
                }
                let ident = self.locals.get(*id);
                let expr = gen_value(*value, &mut self.locals, &self.vars);
                let assign = util::passign(es::AssignOp::Assign, ident, expr);
                stmts.push(util::wrap(assign));
            }
        }
    }

    fn finish(self, nargs: u32) -> Option<es::Decl> {
        let mut vars = Vec::new();
        for (iid, name) in self.locals.ident {
            if iid >= nargs {
                vars.push(util::vari(name, None));
            }
        }
        if vars.is_empty() {
            return None;
        }
        Some(util::var(vars))
    }
}

impl<'a> BlockCodegen<'a> {
    fn new(codegen: &'a mut Codegen, body: Stmts) -> BlockCodegen<'a> {
        BlockCodegen {
            codegen,
            body,
            stack: Vec::new(),
        }
    }

    fn value(&mut self, value: Value) -> es::Expr {
        if let Value::SSARef(iid) = value {
            if let Some(item) = self.stack.pop() {
                if iid == item.0 {
                    return item.1;
                }
                self.stack.push(item);
            }
        }
        self.codegen.value(value)
    }

    fn values(&mut self, args: &[Value]) -> Vec<es::Expr> {
        let mut exprs: Vec<_> = args.iter().rev().map(|x| self.value(*x)).collect();
        exprs.reverse();
        exprs
    }

    fn field(&mut self, value: Value, member: es::Ident) -> es::Expr {
        let expr = self.value(value);
        util::imember(expr, member)
    }

    fn expr(&mut self, inst: &Instruction) -> es::Expr {
        match inst {
            Instruction::ALoad(_, [target, index]) => {
                let index_expr = self.value(*index);
                let target_expr = self.value(*target);
                util::emember(target_expr, index_expr)
            }
            Instruction::AStore(_, [target, index, value]) => {
                let expr = self.value(*value);
                let index_expr = self.value(*index);
                let target_expr = self.value(*target);
                let dest = util::emember(target_expr, index_expr);
                util::eassign(es::AssignOp::Assign, dest, expr)
            }
            Instruction::Arith(op, typ, [left, right]) => {
                let right_expr = self.value(*right);
                let left_expr = self.value(*left);
                let es_op = match op {
                    ArithOp::Add => es::BinaryOp::Add,
                    ArithOp::Sub => es::BinaryOp::Sub,
                    ArithOp::Mul => es::BinaryOp::Mul,
                    ArithOp::Div => es::BinaryOp::Div,
                    ArithOp::Rem => es::BinaryOp::Mod,
                };
                let expr = util::bin(es_op, left_expr, right_expr);
                gen_trunc(expr, *typ)
            }
            Instruction::Negate(typ, [value]) => {
                let oper_expr = self.value(*value);
                let expr = util::unary(es::UnaryOp::Minus, oper_expr);
                gen_trunc(expr, *typ)
            }
            Instruction::Shift(op, typ, [left, right]) => {
                let right_expr = self.value(*right);
                let left_expr = self.value(*left);
                if *typ {
                    let right_expr_l = gen_conv_long(right_expr);
                    match op {
                        ShiftOp::Shl => gen_ltrunc(util::bin(es::BinaryOp::LShift, left_expr, right_expr_l)),
                        ShiftOp::Shr => util::bin(es::BinaryOp::RShift, left_expr, right_expr_l),
                        ShiftOp::Ushr => gen_ltrunc(util::bin(es::BinaryOp::RShift, gen_ultrunc(left_expr), right_expr_l)),
                    }
                } else {
                    let es_op = match op {
                        ShiftOp::Shl => es::BinaryOp::LShift,
                        ShiftOp::Shr => es::BinaryOp::RShift,
                        ShiftOp::Ushr => es::BinaryOp::ZeroFillRShift,
                    };
                    util::bin(es_op, left_expr, right_expr)
                }
            }
            Instruction::Bit(op, _, [left, right]) => {
                let right_expr = self.value(*right);
                let left_expr = self.value(*left);
                let es_op = match op {
                    BitOp::And => es::BinaryOp::BitAnd,
                    BitOp::Or => es::BinaryOp::BitOr,
                    BitOp::Xor => es::BinaryOp::BitXor,
                };
                util::bin(es_op, left_expr, right_expr)
            }
            Instruction::Conv(op, [value]) => {
                let expr = self.value(*value);
                match op {
                    ConvOp::I2B => util::bin(es::BinaryOp::RShift, util::bin(es::BinaryOp::LShift, expr, 24.0.into()), 24.0.into()),
                    ConvOp::I2C => util::bin(es::BinaryOp::BitAnd, expr, 65535.0.into()),
                    ConvOp::I2S => util::bin(es::BinaryOp::RShift, util::bin(es::BinaryOp::LShift, expr, 16.0.into()), 16.0.into()),
                    ConvOp::I2L => gen_conv_long(expr),
                    ConvOp::L2I => gen_itrunc(gen_conv_number(expr)),
                    ConvOp::L2F => gen_conv_number(expr),
                    ConvOp::L2D => gen_conv_number(expr),
                    ConvOp::D2I => util::bin(es::BinaryOp::BitOr, expr, (0.0).into()),
                    ConvOp::D2F => expr,
                    ConvOp::D2L => gen_conv_long(expr),
                }
            }
            Instruction::ICmp(pred, [left, right]) => {
                let right_expr = self.value(*right);
                let left_expr = self.value(*left);
                let es_op = match pred {
                    Pred::Eq => es::BinaryOp::EqEq,
                    Pred::Ne => es::BinaryOp::NotEq,
                    Pred::Lt => es::BinaryOp::Lt,
                    Pred::Ge => es::BinaryOp::GtEq,
                    Pred::Gt => es::BinaryOp::Gt,
                    Pred::Le => es::BinaryOp::LtEq,
                };
                util::bin(es_op, left_expr, right_expr)
            }
            Instruction::FCmp(pred, [left, right]) => {
                let right_expr = self.value(*right);
                let left_expr = self.value(*left);
                let es_op = match pred {
                    Pred::Eq => es::BinaryOp::EqEq,
                    Pred::Ne => es::BinaryOp::NotEq,
                    Pred::Lt => es::BinaryOp::Lt,
                    Pred::Ge => es::BinaryOp::GtEq,
                    Pred::Gt => es::BinaryOp::Gt,
                    Pred::Le => es::BinaryOp::LtEq,
                };
                util::bin(es_op, left_expr, right_expr)
            }
            Instruction::ACmp(pred, [left, right]) => {
                let right_expr = self.value(*right);
                let left_expr = self.value(*left);
                let es_op = match pred {
                    true => es::BinaryOp::EqEq,
                    false => es::BinaryOp::NotEq,
                };
                util::bin(es_op, left_expr, right_expr)
            }
            Instruction::GetStatic(member) => mangle::global(member.0, member.1).into(),
            Instruction::PutStatic(member, [value]) => {
                let expr = self.value(*value);
                let dest = mangle::global(member.0, member.1).into();
                util::eassign(es::AssignOp::Assign, dest, expr)
            }
            Instruction::GetField(member, [target]) => self.field(*target, mangle::member(member.1)),
            Instruction::PutField(member, [target, value]) => {
                let expr = self.value(*value);
                let dest = self.field(*target, mangle::member(member.1));
                util::eassign(es::AssignOp::Assign, dest, expr)
            }
            Instruction::GetNativeVar(name) => util::ident(name.as_str().into()).into(),
            Instruction::GetNativeProp(name, [target]) => self.field(*target, util::ident(name.as_str().into())),
            Instruction::InvokeVirtual(member, args) => {
                let args_exprs = self.values(&args[1..]);
                let target_expr = self.field(args[0], mangle::member(member.1));
                util::call(target_expr, args_exprs)
            }
            Instruction::InvokeDirect(member, args) | Instruction::InvokeStatic(member, args) => {
                let args_exprs = self.values(args);
                let ident = mangle::global(member.0, member.1);
                util::call(ident.into(), args_exprs)
            }
            Instruction::InvokeNativeFunc(name, isasync, args) => {
                self.codegen.hasawait |= *isasync;
                let args_exprs = self.values(args);
                let ident = util::ident(name.as_str().into());
                util::maybeawait(*isasync, util::call(ident.into(), args_exprs))
            }
            Instruction::InvokeNativeProp(name, isasync, args) => {
                self.codegen.hasawait |= *isasync;
                let args_exprs = self.values(&args[1..]);
                let target_expr = self.field(args[0], util::ident(name.as_str().into()));
                util::maybeawait(*isasync, util::call(target_expr, args_exprs))
            }
            Instruction::New(cid) => util::new(mangle::class_e(*cid), vec![]),
            Instruction::NewArray(cid, values) => match values[..] {
                [value] => {
                    let expr = self.value(value);
                    let class = mangle::class_e(*cid);
                    if cid.len() == 2 {
                        util::new(class, vec![expr])
                    } else {
                        util::call(util::ident("$javanewarray".into()).into(), vec![class, expr])
                    }
                }
                _ => {
                    let mut exprs = self.values(values);
                    exprs.insert(0, mangle::class_e(*cid));
                    if cid.len() == 2 {
                        util::call(util::ident("$javamultiprimarray".into()).into(), exprs)
                    } else {
                        util::call(util::ident("$javamultinewarray".into()).into(), exprs)
                    }
                }
            },
            Instruction::InstanceOf(class, [value]) => {
                let expr = self.value(*value);
                if *class == "java/lang/Object" {
                    util::bin(es::BinaryOp::NotEq, expr, es::Lit::Null(util::NULL).into())
                } else {
                    let object_expr = util::call(util::ident("Object".into()).into(), vec![expr]);
                    let class_expr = util::imember(object_expr, util::ident("constructor".into()));
                    gen_itrunc(util::imember(class_expr, mangle::instance(*class, 1)))
                }
            }
            Instruction::NativeToString([value]) => {
                let expr = self.value(*value);
                util::bin(es::BinaryOp::Add, expr, quote_str!("").into())
            }
        }
    }

    fn flush(&mut self) {
        let stack = mem::take(&mut self.stack);
        for (iid, expr) in stack {
            let iid = self.codegen.vars.get(&iid).copied().unwrap_or(iid);
            let assign = util::passign(es::AssignOp::Assign, self.codegen.ident(iid), expr);
            self.body.push(util::wrap(assign));
        }
    }

    fn instructions(&mut self, instructions: &[(u32, Instruction)]) {
        for (iid, inst) in instructions {
            let expr = self.expr(inst);
            match self.codegen.uses.get(iid) {
                Some(true) => {
                    self.flush();
                    let iid = self.codegen.vars.get(iid).copied().unwrap_or(*iid);
                    let assign = util::passign(es::AssignOp::Assign, self.codegen.ident(iid), expr);
                    self.body.push(util::wrap(assign));
                }
                Some(false) => {
                    self.stack.push((*iid, expr));
                }
                None => {
                    self.flush();
                    self.body.push(util::wrap(expr));
                }
            }
        }
    }

    fn movephis(&mut self, from: BlockId, to: BlockId) {
        self.codegen.movephis(&mut self.body, from, to);
    }

    fn finish(mut self) -> Stmts {
        self.flush();
        self.body
    }
}

fn generate_shaped_block(body: Stmts, cg: &mut Codegen, func: &Function, shaped: &Shaped) -> Stmts {
    match shaped {
        ShapedBlock::Simple(block) => generate_shaped_block_simple(body, cg, func, block),
        ShapedBlock::Loop(block) => generate_shaped_block_loop(body, cg, func, block),
        ShapedBlock::Multiple(block) => generate_shaped_block_multiple(body, cg, func, block),
    }
}

fn generate_shaped_block_maybe(body: Stmts, cg: &mut Codegen, func: &Function, shaped: &Option<Box<Shaped>>) -> Stmts {
    match shaped.as_ref() {
        Some(shaped) => generate_shaped_block(body, cg, func, shaped),
        None => body,
    }
}

fn generate_shaped_block_simple(mut body: Stmts, cg: &mut Codegen, func: &Function, simple: &Simple) -> Stmts {
    let block = &func.blocks[&simple.label];
    let mut bcg = BlockCodegen::new(cg, body);
    bcg.instructions(&block.instructions);
    match block.terminator {
        Terminator::Return([Value::Undef]) => {
            bcg.flush();
            bcg.body.push(util::returns(None));
            body = bcg.finish();
            assert!(simple.branches.is_empty());
            assert!(simple.immediate.is_none());
            assert!(simple.next.is_none());
        }
        Terminator::Return([value]) => {
            let expr = bcg.value(value);
            bcg.flush();
            bcg.body.push(util::returns(Some(expr)));
            body = bcg.finish();
            assert!(simple.branches.is_empty());
            assert!(simple.immediate.is_none());
            assert!(simple.next.is_none());
        }
        Terminator::Throw([value]) => {
            let expr = bcg.value(value);
            bcg.flush();
            bcg.body.push(util::throw(expr));
            body = bcg.finish();
            assert!(simple.branches.is_empty());
            assert!(simple.immediate.is_none());
            assert!(simple.next.is_none());
        }
        Terminator::Goto([next]) => {
            bcg.flush();
            bcg.movephis(simple.label, next);
            body = bcg.finish();
            match simple.branches.get(&next) {
                Some(branch) => {
                    cg.branchmode(&mut body, *branch, next);
                    assert!(simple.immediate.is_none());
                }
                None => {
                    match simple.immediate.as_deref() {
                        Some(ShapedBlock::Simple(_)) => {}
                        Some(ShapedBlock::Loop(lblock)) => match lblock.inner.as_ref() {
                            ShapedBlock::Simple(_) => {}
                            _ => panic!("unexpected block in loop after goto"),
                        },
                        Some(ShapedBlock::Multiple(_)) => {
                            panic!("unexpected multiple after goto")
                        }
                        None => panic!("unexpected none after goto"),
                    }
                    body = generate_shaped_block_maybe(body, cg, func, &simple.immediate);
                }
            }
            body = generate_shaped_block_maybe(body, cg, func, &simple.next);
        }
        Terminator::GotoIf([value], [next1, next0]) => {
            let handled = match simple.immediate.as_deref() {
                Some(ShapedBlock::Multiple(mblock)) => &mblock.handled[..],
                None => &[],
                _ => panic!("unsupported"),
            };
            let expr = bcg.value(value);
            bcg.flush();
            body = bcg.finish();
            let mut remap = |next| {
                let mut inner = Vec::new();
                cg.movephis(&mut inner, simple.label, next);
                match handled.iter().find(|x| x.labels.contains(&next)) {
                    Some(hblock) => inner = generate_shaped_block(inner, cg, func, &hblock.inner),
                    None => cg.branchmode(&mut inner, simple.branches[&next], next),
                }
                inner
            };
            let inner1 = remap(next1);
            let inner0 = remap(next0);
            gen_if(&mut body, expr, inner1, inner0);
            body = generate_shaped_block_maybe(body, cg, func, &simple.next);
        }
        Terminator::Switch([value], ref nexts, ref values) => {
            let handled = match simple.immediate.as_deref() {
                Some(ShapedBlock::Multiple(mblock)) => &mblock.handled[..],
                None => &[],
                _ => panic!("unsupported"),
            };
            let expr = bcg.value(value);
            bcg.flush();
            body = bcg.finish();
            let mut remap = |next, cond: Option<f64>| {
                let mut inner = Vec::new();
                cg.movephis(&mut inner, simple.label, next);
                match handled.iter().find(|x| x.labels.contains(&next)) {
                    Some(hblock) => inner = generate_shaped_block(inner, cg, func, &hblock.inner),
                    None => cg.branchmode(&mut inner, simple.branches[&next], next),
                }
                if !is_nonlocal_flow(&inner) {
                    inner.push(util::lbreak(None));
                }
                util::switchcase(cond.map(es::Expr::from), inner)
            };
            let mut cases = Vec::new();
            for (next, cond) in nexts.iter().zip(values.iter()) {
                cases.push(remap(*next, Some(*cond as f64)));
            }
            cases.push(remap(nexts[values.len()], None));
            body.push(util::switch(expr, cases));
            body = generate_shaped_block_maybe(body, cg, func, &simple.next);
        }
    }
    body
}

fn generate_shaped_block_loop(mut body: Stmts, cg: &mut Codegen, func: &Function, sloop: &Loop) -> Stmts {
    let mut inner = generate_shaped_block(vec![], cg, func, &sloop.inner);
    if !is_nonlocal_flow(&inner) {
        inner.push(util::lbreak(None));
    }
    let mut stmt = util::loops(util::block(inner));
    if let Some(label) = cg.labels.ident.get(&(sloop.loop_id as u32)) {
        stmt = util::labeled(label.clone(), stmt);
    }
    body.push(stmt);
    body = generate_shaped_block_maybe(body, cg, func, &sloop.next);
    body
}

fn generate_shaped_block_multiple(mut body: Stmts, cg: &mut Codegen, func: &Function, multiple: &Multiple) -> Stmts {
    let mut cases = Vec::new();
    for hblock in &multiple.handled {
        if let Some((last, rest)) = hblock.labels.split_last() {
            for label in rest {
                cases.push(util::switchcase(Some(f64::from(label.0).into()), vec![]));
            }
            let mut inner = generate_shaped_block(vec![], cg, func, &hblock.inner);
            if hblock.break_after && !is_nonlocal_flow(&inner) {
                inner.push(util::lbreak(None));
            }
            cases.push(util::switchcase(Some(f64::from(last.0).into()), inner));
        }
    }
    let stmt = util::switch(cg.ident(!0).into(), cases);
    body.push(stmt);
    body
}

fn collect_uses(func: &Function) -> UseMap {
    let mut uses = HashMap::new();
    for block in func.blocks.values() {
        let preds = &block.preds;
        for (_, inst) in &block.instructions {
            for value in inst.operands() {
                if let Value::SSARef(iid) = value {
                    let contains = uses.contains_key(iid);
                    uses.insert(*iid, contains || !block.has_instruction(*iid));
                }
            }
        }
        for value in block.terminator.operands() {
            if let Value::SSARef(iid) = value {
                let contains = uses.contains_key(iid);
                uses.insert(*iid, contains || !block.has_instruction(*iid));
            }
        }
        for values in block.phis.values() {
            for (from, value) in preds.iter().zip(values.as_ref()) {
                if let Value::SSARef(iid) = value {
                    let contains = uses.contains_key(iid);
                    uses.insert(*iid, contains || !func.blocks[from].has_instruction(*iid));
                }
            }
        }
    }
    uses
}

fn collect_phis(func: &Function) -> PhiMap {
    let mut map = HashMap::new();
    for (to, block) in &func.blocks {
        let preds = &block.preds;
        for (id, values) in &block.phis {
            for (from, value) in preds.iter().zip(values.as_ref()) {
                if *value != Value::SSARef(*id) {
                    map.push((*from, *to), (*id, *value));
                }
            }
        }
    }
    for phis in map.values_mut() {
        phis.sort_by_key(|x| x.0);
    }
    map
}

fn merge_simple_phis(func: &Function, uses: &UseMap, phis: &PhiMap) -> VarMap {
    let mut map = HashMap::new();
    for (from, block) in &func.blocks {
        if let Terminator::Goto([to]) = block.terminator {
            if let Some(moves) = phis.get(&(*from, to)) {
                for (dest, src) in moves {
                    if let Value::SSARef(sid) = src {
                        if uses[sid] {
                            continue;
                        }
                        let mut ok = false;
                        for (iid, inst) in (&block.instructions).iter().rev() {
                            if *iid == *sid {
                                ok = true;
                                break;
                            }
                            if inst.operands().contains(&Value::SSARef(*dest)) {
                                break;
                            }
                        }
                        if ok {
                            map.insert(*sid, *dest);
                        }
                    }
                }
            }
        }
    }
    map
}

pub fn generate(func: &Function) -> es::Function {
    let uses = collect_uses(func);
    let phis = collect_phis(func);
    let vars = merge_simple_phis(func, &uses, &phis);
    let mut cg = Codegen::new(uses, phis, vars);
    let params = (0..func.nargs).map(|i| util::param(cg.ident(i))).collect();
    let blocks = func.succs();
    let shaped = relooper::reloop(blocks, func.entry);
    let mut body = generate_shaped_block(vec![], &mut cg, func, &shaped);
    if let Some(es::Stmt::Return(stmt)) = body.last() {
        if stmt.arg.is_none() {
            body.pop();
        }
    }
    let isasync = cg.hasawait;
    if let Some(var_decl) = cg.finish(func.nargs) {
        body.insert(0, var_decl.into());
    }
    util::function(params, body, isasync)
}

fn generate_proxy_args(params: &mut Vec<es::Param>, args: &mut Vec<es::Expr>, desc: Ustr) -> bool {
    let parsed = types::parse(desc).unwrap_or_default();
    let mut next: usize = 0;
    for _ in 0..parsed.0.len() {
        let ident = mangle::ident(&mut next);
        params.push(util::param(ident.clone()));
        args.push(ident.into());
    }
    parsed.1.is_some()
}

pub fn generate_proxy(class: Ustr, nametype: NameAndType) -> es::Function {
    let mut params = Vec::new();
    let mut args = vec![util::THIS.into()];
    let nonvoid = generate_proxy_args(&mut params, &mut args, nametype.1);
    let ident = mangle::global(class, nametype);
    let expr = util::call(ident.into(), args);
    match nonvoid {
        true => util::function(params, vec![util::returns(Some(expr))], false),
        false => util::function(params, vec![util::wrap(expr)], false),
    }
}

pub fn generate_native_proxy(native: Native, nametype: NameAndType) -> es::Function {
    let mut params = Vec::new();
    let mut nonvoid = true;
    let expr = match native {
        Native::None => {
            return util::function(params, vec![], false);
        }
        Native::IsEqual => {
            let mut next: usize = 0;
            let ident = mangle::ident(&mut next);
            params.push(util::param(ident.clone()));
            util::bin(es::BinaryOp::EqEq, util::THIS.into(), ident.into())
        }
        Native::ToString => util::bin(es::BinaryOp::Add, util::THIS.into(), quote_str!("").into()),
        Native::Var(name) => util::ident(name.as_str().into()).into(),
        Native::Func(name) | Native::AsyncFunc(name) => {
            let mut args = vec![util::THIS.into()];
            nonvoid = generate_proxy_args(&mut params, &mut args, nametype.1);
            let ident = util::ident(name.as_str().into());
            util::call(ident.into(), args)
        }
        Native::Prop(name) => {
            let ident = util::ident(name.as_str().into());
            util::imember(util::THIS.into(), ident)
        }
        Native::Method(name) | Native::AsyncMethod(name) => {
            let mut args = Vec::new();
            nonvoid = generate_proxy_args(&mut params, &mut args, nametype.1);
            let ident = util::ident(name.as_str().into());
            util::call(util::imember(util::THIS.into(), ident), args)
        }
    };
    match nonvoid {
        true => util::function(params, vec![util::returns(Some(expr))], false),
        false => util::function(params, vec![util::wrap(expr)], false),
    }
}
