use noak::reader::attributes::ArrayType;
use std::collections::{HashMap, HashSet};
use ustr::Ustr;

use crate::ir::function::{ArithOp, BasicBlock, BlockId, ConvOp, Function, Instruction, NumType, Pred, Push, Terminator, Value, Values};
use crate::ir::module::Member;
use crate::parser::code::{CmpType, Inst, TruncType};
use crate::parser::flow::{Block, FindBlock, PredList};
use crate::parser::types;

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
enum VarId {
    Local(u16),
    Stack(u16),
}

struct Builder<'a> {
    func: Function,
    phis: HashMap<u32, Values>,
    phi_users: HashMap<u32, HashSet<u32>>,
    incomplete: HashMap<BlockId, Vec<(u32, VarId)>>,
    trivial_phis: HashMap<u32, Value>,
    block_phis: HashMap<BlockId, Vec<u32>>,
    values: HashMap<(BlockId, VarId), Value>,
    missing_preds: HashMap<BlockId, HashSet<BlockId>>,
    preds: &'a PredList,
}

struct BuilderBlock<'a, 'b> {
    builder: &'a mut Builder<'b>,
    instructions: Vec<(u32, Instruction)>,
    block_id: BlockId,
    stack_top: u16,
}

pub enum Error {
    BadType,
    StackOverflow,
    StackUnderflow,
    StackHeightMismatch,
}

fn is_trivial_phi(iid: u32, values: &[Value]) -> Option<Value> {
    let mut same = Value::Undef;
    for value in values {
        if *value == same || *value == Value::SSARef(iid) {
            continue;
        }
        if same != Value::Undef {
            return None;
        }
        same = *value;
    }
    Some(same)
}

impl<'a> Builder<'a> {
    fn new(preds: &'a PredList, nargs: u32) -> Builder<'a> {
        let func = Function::new(nargs);
        let missing_preds = preds.iter().map(|(i, x)| (*i, x.iter().cloned().collect())).collect();
        Builder {
            func,
            phis: HashMap::new(),
            phi_users: HashMap::new(),
            block_phis: HashMap::new(),
            incomplete: HashMap::new(),
            trivial_phis: HashMap::new(),
            values: HashMap::new(),
            missing_preds,
            preds,
        }
    }

    fn fill_entry(&mut self, parameters: &[bool], is_static: bool) {
        let mut index: u16 = 0;
        let mut iid: u32 = 0;
        if !is_static {
            self.values.insert((BlockId(!0), VarId::Local(index)), Value::SSARef(iid));
            index += 1;
            iid += 1;
        }
        for wide in parameters {
            self.values.insert((BlockId(!0), VarId::Local(index)), Value::SSARef(iid));
            index += (*wide as u16) + 1;
            iid += 1;
        }
        self.add_filled_pred(BlockId(!0), BlockId(0));
    }

    fn add_filled_pred(&mut self, from: BlockId, to: BlockId) {
        let missing = self.missing_preds.get_mut(&to).unwrap();
        if missing.remove(&from) && missing.is_empty() {
            if let Some(phis) = self.incomplete.remove(&to) {
                for (iid, index) in phis {
                    let preds = &self.preds[&to];
                    self.add_phi_operands(iid, index, preds);
                }
            }
        }
    }

    fn is_sealed(&self, block: BlockId) -> bool {
        self.missing_preds[&block].is_empty()
    }

    fn put(&mut self, block: BlockId, index: VarId, value: Value) {
        self.values.insert((block, index), value);
    }

    fn get(&mut self, block: BlockId, index: VarId) -> Value {
        if let Some(value) = self.values.get(&(block, index)) {
            return *value;
        }
        let preds = &self.preds[&block];
        if preds.is_empty() {
            return Value::Undef;
        }
        if !self.is_sealed(block) {
            let iid = self.add_phi_node(block);
            self.incomplete.push(block, (iid, index));
            self.values.insert((block, index), Value::SSARef(iid));
            return Value::SSARef(iid);
        }
        if preds.len() == 1 {
            let pred = preds[0];
            let value = self.get(pred, index);
            self.values.insert((block, index), value);
            return value;
        }
        let iid = self.add_phi_node(block);
        self.values.insert((block, index), Value::SSARef(iid));
        self.add_phi_operands(iid, index, preds)
    }

    fn add_phi_node(&mut self, block: BlockId) -> u32 {
        let iid = self.func.nextid();
        self.phi_users.insert(iid, HashSet::new());
        self.block_phis.push(block, iid);
        iid
    }

    fn add_phi_operands(&mut self, iid: u32, index: VarId, preds: &[BlockId]) -> Value {
        let mut values: Values = preds.iter().map(|id| self.get(*id, index)).collect();
        for value in values.as_mut() {
            value.replace_if_mapped(&self.trivial_phis);
            self.add_phi_user(iid, *value);
        }
        let one_value = is_trivial_phi(iid, &values);
        if let Some(value) = one_value {
            self.replace_all_phi_uses(iid, value);
            return value;
        }
        self.phis.insert(iid, values);
        Value::SSARef(iid)
    }

    fn add_phi_user(&mut self, iid: u32, value: Value) {
        if value == Value::SSARef(iid) {
            return;
        }
        if let Value::SSARef(sid) = value {
            if let Some(set) = self.phi_users.get_mut(&sid) {
                set.insert(iid);
            }
        }
    }

    fn replace_all_phi_uses(&mut self, iid: u32, value: Value) {
        self.trivial_phis.insert(iid, value);
        let users = self.phi_users.remove(&iid).unwrap();
        for phiid in &users {
            if let Some(phi) = self.phis.get_mut(phiid) {
                for item in phi.as_mut() {
                    item.replace_if(iid, value);
                }
            }
            if let Some(phi) = self.trivial_phis.get_mut(phiid) {
                assert!(*phi == Value::SSARef(iid));
                *phi = value;
            }
            self.add_phi_user(*phiid, value);
        }
        for phiid in &users {
            if let Some(phi) = self.phis.get_mut(phiid) {
                let one_value = is_trivial_phi(iid, phi);
                if let Some(value) = one_value {
                    self.phis.remove(phiid);
                    self.replace_all_phi_uses(*phiid, value);
                }
            }
        }
    }

    fn fill(mut self) -> Function {
        for (block, phis) in self.block_phis {
            let bb = self.func.blocks.get_mut(&block).unwrap();
            for iid in &phis {
                if let Some(phi) = self.phis.remove(iid) {
                    bb.phis.insert(*iid, phi);
                }
            }
        }
        for (_, bb) in self.func.blocks.iter_mut() {
            for (_, inst) in &mut bb.instructions {
                for oper in inst.operands_mut() {
                    oper.replace_if_mapped(&self.trivial_phis);
                }
            }
            for oper in bb.terminator.operands_mut() {
                oper.replace_if_mapped(&self.trivial_phis);
            }
        }
        self.func
    }
}

impl<'a, 'b> BuilderBlock<'a, 'b> {
    fn new(builder: &'a mut Builder<'b>, block_id: BlockId, stack_top: u16) -> BuilderBlock<'a, 'b> {
        BuilderBlock {
            builder,
            instructions: Vec::new(),
            block_id,
            stack_top,
        }
    }

    fn push(&mut self, value: Value, wide: bool) -> Result<(), Error> {
        self.put(VarId::Stack(self.stack_top), value);
        self.stack_top = self.stack_top.checked_add(1).ok_or(Error::StackOverflow)?;
        if wide {
            self.put(VarId::Stack(self.stack_top), Value::Undef);
            self.stack_top = self.stack_top.checked_add(1).ok_or(Error::StackOverflow)?;
        }
        Ok(())
    }

    fn discard(&mut self, wide: bool) -> Result<(), Error> {
        self.stack_top = self.stack_top.checked_sub((wide as u16) + 1).ok_or(Error::StackUnderflow)?;
        Ok(())
    }

    fn pop(&mut self, wide: bool) -> Result<Value, Error> {
        self.discard(wide)?;
        Ok(self.get(VarId::Stack(self.stack_top)))
    }

    fn pop_many(&mut self, wide: &[bool], extra: bool) -> Result<Box<[Value]>, Error> {
        let mut values = Vec::new();
        for w in wide.iter().rev() {
            values.push(self.pop(*w)?);
        }
        if extra {
            values.push(self.pop(false)?);
        }
        values.reverse();
        Ok(values.into_boxed_slice())
    }

    fn put(&mut self, index: VarId, value: Value) {
        self.builder.put(self.block_id, index, value);
    }

    fn get(&mut self, index: VarId) -> Value {
        self.builder.get(self.block_id, index)
    }

    fn add(&mut self, inst: Instruction) -> Value {
        let iid = self.builder.func.nextid();
        self.instructions.push((iid, inst));
        Value::SSARef(iid)
    }

    fn fill(self, term: Terminator) {
        for succ in term.succs() {
            self.builder.add_filled_pred(self.block_id, *succ);
        }
        let block = BasicBlock {
            preds: self.builder.preds[&self.block_id].clone(),
            phis: HashMap::new(),
            instructions: self.instructions,
            terminator: term,
        };
        self.builder.func.blocks.insert(self.block_id, block);
    }
}

pub fn build(blocks: &[Block], order: &[BlockId], preds: &PredList, mtype: Ustr, is_static: bool) -> Result<Function, Error> {
    let method_type = types::parse(mtype).ok_or(Error::BadType)?;
    let nargs = method_type.0.len() as u32 + !is_static as u32;
    let mut builder = Builder::new(preds, nargs);
    let mut stack_on_entry = HashMap::new();
    stack_on_entry.insert(BlockId(0), 0);
    builder.fill_entry(&method_type.0, is_static);
    for block_id in order {
        let block_index = blocks.find(block_id.0).unwrap();
        let inst_list = blocks[block_index];
        let mut builder = BuilderBlock::new(&mut builder, *block_id, stack_on_entry[block_id]);
        for (_, inst) in inst_list {
            match inst {
                Inst::Nop => {}
                Inst::AConstNull => builder.push(Value::Null, false)?,
                Inst::IConst(val) => builder.push(Value::Integer(*val), false)?,
                Inst::LConst(val) => builder.push(Value::Long(*val), true)?,
                Inst::FConst(val) => builder.push(Value::Float(val.to_bits()), false)?,
                Inst::DConst(val) => builder.push(Value::Double(val.to_bits()), true)?,
                Inst::CConst(val) => builder.push(Value::Class(*val), false)?,
                Inst::SConst(val) => builder.push(Value::String(*val), false)?,
                Inst::Load(index, wide) => {
                    let value = builder.get(VarId::Local(*index));
                    builder.push(value, *wide)?;
                }
                Inst::Store(index, wide) => {
                    let value = builder.pop(*wide)?;
                    builder.put(VarId::Local(*index), value);
                }
                Inst::ALoad(typ) => {
                    let wide = *typ == Some(ArrayType::Long) || *typ == Some(ArrayType::Double);
                    let index = builder.pop(false)?;
                    let target = builder.pop(false)?;
                    let value = builder.add(Instruction::ALoad(*typ, [target, index]));
                    builder.push(value, wide)?;
                }
                Inst::AStore(typ) => {
                    let wide = *typ == Some(ArrayType::Long) || *typ == Some(ArrayType::Double);
                    let value = builder.pop(wide)?;
                    let index = builder.pop(false)?;
                    let target = builder.pop(false)?;
                    builder.add(Instruction::AStore(*typ, [target, index, value]));
                }
                Inst::Pop(wide) => builder.discard(*wide)?,
                Inst::Dup(false) => {
                    let value = builder.pop(false)?;
                    builder.push(value, false)?;
                    builder.push(value, false)?;
                }
                Inst::Dup(true) => {
                    let value1 = builder.pop(false)?;
                    let value2 = builder.pop(false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                }
                Inst::DupX1(false) => {
                    let value1 = builder.pop(false)?;
                    let value2 = builder.pop(false)?;
                    builder.push(value1, false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                }
                Inst::DupX1(true) => {
                    let value1 = builder.pop(false)?;
                    let value2 = builder.pop(false)?;
                    let value3 = builder.pop(false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                    builder.push(value3, false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                }
                Inst::DupX2(false) => {
                    let value1 = builder.pop(false)?;
                    let value2 = builder.pop(false)?;
                    let value3 = builder.pop(false)?;
                    builder.push(value1, false)?;
                    builder.push(value3, false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                }
                Inst::DupX2(true) => {
                    let value1 = builder.pop(false)?;
                    let value2 = builder.pop(false)?;
                    let value3 = builder.pop(false)?;
                    let value4 = builder.pop(false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                    builder.push(value4, false)?;
                    builder.push(value3, false)?;
                    builder.push(value2, false)?;
                    builder.push(value1, false)?;
                }
                Inst::Swap => {
                    let value1 = builder.pop(false)?;
                    let value2 = builder.pop(false)?;
                    builder.push(value1, false)?;
                    builder.push(value2, false)?;
                }
                Inst::Arith(op, typ) => {
                    let rhs = builder.pop(typ.wide())?;
                    let lhs = builder.pop(typ.wide())?;
                    let value = builder.add(Instruction::Arith(*op, *typ, [lhs, rhs]));
                    builder.push(value, typ.wide())?;
                }
                Inst::Neg(typ) => {
                    let oper = builder.pop(typ.wide())?;
                    let value = builder.add(Instruction::Negate(*typ, [oper]));
                    builder.push(value, typ.wide())?;
                }
                Inst::IInc(index, val) => {
                    let lhs = builder.get(VarId::Local(*index));
                    let rhs = Value::Integer(*val as i32);
                    let inst = Instruction::Arith(ArithOp::Add, NumType::Int, [lhs, rhs]);
                    let value = builder.add(inst);
                    builder.put(VarId::Local(*index), value);
                }
                Inst::Shift(op, wide) => {
                    let rhs = builder.pop(false)?;
                    let lhs = builder.pop(*wide)?;
                    let value = builder.add(Instruction::Shift(*op, *wide, [lhs, rhs]));
                    builder.push(value, *wide)?;
                }
                Inst::Bit(op, wide) => {
                    let rhs = builder.pop(*wide)?;
                    let lhs = builder.pop(*wide)?;
                    let value = builder.add(Instruction::Bit(*op, *wide, [lhs, rhs]));
                    builder.push(value, *wide)?;
                }
                Inst::Conv(ityp, otyp) => {
                    let oper = builder.pop(ityp.wide())?;
                    let value = match (ityp, otyp) {
                        (NumType::Int, NumType::Long) => builder.add(Instruction::Conv(ConvOp::I2L, [oper])),
                        (NumType::Long, NumType::Int) => builder.add(Instruction::Conv(ConvOp::L2I, [oper])),
                        (NumType::Long, NumType::Float) => builder.add(Instruction::Conv(ConvOp::L2F, [oper])),
                        (NumType::Long, NumType::Double) => builder.add(Instruction::Conv(ConvOp::L2D, [oper])),
                        (_, NumType::Int) => builder.add(Instruction::Conv(ConvOp::D2I, [oper])),
                        (_, NumType::Long) => builder.add(Instruction::Conv(ConvOp::D2L, [oper])),
                        (_, NumType::Float) => builder.add(Instruction::Conv(ConvOp::D2F, [oper])),
                        (_, NumType::Double) => oper,
                    };
                    builder.push(value, otyp.wide())?;
                }
                Inst::Trunc(typ) => {
                    let oper = builder.pop(false)?;
                    let value = match typ {
                        TruncType::Byte => builder.add(Instruction::Conv(ConvOp::I2B, [oper])),
                        TruncType::Char => builder.add(Instruction::Conv(ConvOp::I2C, [oper])),
                        TruncType::Short => builder.add(Instruction::Conv(ConvOp::I2S, [oper])),
                    };
                    builder.push(value, false)?;
                }
                Inst::GotoIf(_, _, _) => {}
                Inst::Goto(_) => {}
                Inst::Jsr(_) => {}
                Inst::Ret(_) => {}
                Inst::Switch(_, _) => {}
                Inst::Return(_) => {}
                Inst::GetStatic(class, name) => {
                    let member = Member(*class, *name);
                    let value = builder.add(Instruction::GetStatic(member));
                    builder.push(value, name.1 == "J" || name.1 == "D")?;
                }
                Inst::PutStatic(class, name) => {
                    let member = Member(*class, *name);
                    let oper = builder.pop(name.1 == "J" || name.1 == "D")?;
                    builder.add(Instruction::PutStatic(member, [oper]));
                }
                Inst::GetField(class, name) => {
                    let member = Member(*class, *name);
                    let target = builder.pop(false)?;
                    let value = builder.add(Instruction::GetField(member, [target]));
                    builder.push(value, name.1 == "J" || name.1 == "D")?;
                }
                Inst::PutField(class, name) => {
                    let member = Member(*class, *name);
                    let oper = builder.pop(name.1 == "J" || name.1 == "D")?;
                    let target = builder.pop(false)?;
                    builder.add(Instruction::PutField(member, [target, oper]));
                }
                Inst::InvokeVirtual(class, name) => {
                    let member = Member(*class, *name);
                    let desc = types::parse(name.1).ok_or(Error::BadType)?;
                    let args = builder.pop_many(&desc.0, true)?;
                    let value = builder.add(Instruction::InvokeVirtual(member, args));
                    if let Some(wide) = desc.1 {
                        builder.push(value, wide)?;
                    }
                }
                Inst::InvokeSpecial(class, name) => {
                    let member = Member(*class, *name);
                    let desc = types::parse(name.1).ok_or(Error::BadType)?;
                    let args = builder.pop_many(&desc.0, true)?;
                    let value = builder.add(Instruction::InvokeDirect(member, args));
                    if let Some(wide) = desc.1 {
                        builder.push(value, wide)?;
                    }
                }
                Inst::InvokeStatic(class, name) => {
                    let member = Member(*class, *name);
                    let desc = types::parse(name.1).ok_or(Error::BadType)?;
                    let args = builder.pop_many(&desc.0, false)?;
                    let value = builder.add(Instruction::InvokeStatic(member, args));
                    if let Some(wide) = desc.1 {
                        builder.push(value, wide)?;
                    }
                }
                Inst::InvokeDynamic(_, name) => {
                    let desc = types::parse(name.1).ok_or(Error::BadType)?;
                    builder.pop_many(&desc.0, false)?;
                    if let Some(wide) = desc.1 {
                        builder.push(Value::Undef, wide)?;
                    }
                }
                Inst::New(cid) => {
                    let value = builder.add(Instruction::New(*cid));
                    builder.push(value, false)?;
                }
                Inst::NewArray(typ) => {
                    let cid = match typ {
                        ArrayType::Boolean => "[Z",
                        ArrayType::Char => "[C",
                        ArrayType::Float => "[F",
                        ArrayType::Double => "[D",
                        ArrayType::Byte => "[B",
                        ArrayType::Short => "[S",
                        ArrayType::Int => "[I",
                        ArrayType::Long => "[J",
                    };
                    let size = builder.pop(false)?;
                    let value = builder.add(Instruction::NewArray(cid.into(), [size].into()));
                    builder.push(value, false)?;
                }
                Inst::ANewArray(cid) => {
                    let atype = if cid.starts_with('[') {
                        "[".to_string() + cid.as_str()
                    } else {
                        "[L".to_string() + cid.as_str() + ";"
                    };
                    let size = builder.pop(false)?;
                    let value = builder.add(Instruction::NewArray(atype.into(), [size].into()));
                    builder.push(value, false)?;
                }
                Inst::ArrayLength => {
                    let oper = builder.pop(false)?;
                    let value = builder.add(Instruction::GetNativeProp("length".into(), [oper]));
                    builder.push(value, false)?;
                }
                Inst::AThrow => {}
                Inst::CheckCast(_) => {}
                Inst::InstanceOf(cid) => {
                    let oper = builder.pop(false)?;
                    let value = builder.add(Instruction::InstanceOf(*cid, [oper]));
                    builder.push(value, false)?;
                }
                Inst::Monitor(_) => {
                    builder.discard(false)?;
                }
                Inst::MultiANewArray(cid, dims) => {
                    let sizes: Vec<Value> = (0..*dims).map(|_| builder.pop(false)).collect::<Result<_, _>>()?;
                    let value = builder.add(Instruction::NewArray(*cid, sizes.into()));
                    builder.push(value, false)?;
                }
            };
        }
        let term = match &inst_list.last().unwrap().1 {
            Inst::GotoIf(pred, typ, dest) => {
                let next = blocks.get_start(block_index + 1);
                let mut flip = false;
                let value = match typ {
                    CmpType::ICmp0 => {
                        let oper = builder.pop(false)?;
                        match pred {
                            Pred::Eq => {
                                flip = true;
                                oper
                            }
                            Pred::Ne => oper,
                            _ => builder.add(Instruction::ICmp(*pred, [oper, Value::Integer(0)])),
                        }
                    }
                    CmpType::ICmp => {
                        let rhs = builder.pop(false)?;
                        let lhs = builder.pop(false)?;
                        builder.add(Instruction::ICmp(*pred, [lhs, rhs]))
                    }
                    CmpType::LCmp => {
                        let rhs = builder.pop(true)?;
                        let lhs = builder.pop(true)?;
                        builder.add(Instruction::ICmp(*pred, [lhs, rhs]))
                    }
                    CmpType::FCmpL | CmpType::FCmpG => {
                        let rhs = builder.pop(false)?;
                        let lhs = builder.pop(false)?;
                        builder.add(Instruction::FCmp(*pred, [lhs, rhs]))
                    }
                    CmpType::DCmpL | CmpType::DCmpG => {
                        let rhs = builder.pop(true)?;
                        let lhs = builder.pop(true)?;
                        builder.add(Instruction::FCmp(*pred, [lhs, rhs]))
                    }
                    CmpType::ACmp => {
                        let rhs = builder.pop(false)?;
                        let lhs = builder.pop(false)?;
                        builder.add(Instruction::ACmp(*pred == Pred::Eq, [lhs, rhs]))
                    }
                    CmpType::ACmp0 => {
                        let oper = builder.pop(false)?;
                        builder.add(Instruction::ACmp(*pred == Pred::Eq, [oper, Value::Null]))
                    }
                };
                match flip {
                    false => Terminator::GotoIf([value], [BlockId(*dest), BlockId(next)]),
                    true => Terminator::GotoIf([value], [BlockId(next), BlockId(*dest)]),
                }
            }
            Inst::Goto(dest) => Terminator::Goto([BlockId(*dest)]),
            Inst::Jsr(_) | Inst::Ret(_) => Terminator::Throw([Value::Null]),
            Inst::Switch(dest, cases) => {
                let value = builder.pop(false)?;
                let mut next: Vec<_> = cases.iter().map(|(_, x)| BlockId(*x)).collect();
                let values = cases.iter().map(|(x, _)| *x).collect();
                next.push(BlockId(*dest));
                Terminator::Switch([value], next.into_boxed_slice(), values)
            }
            Inst::Return(Some(wide)) => Terminator::Return([builder.pop(*wide)?]),
            Inst::Return(None) => Terminator::Return([Value::Undef]),
            Inst::AThrow => Terminator::Throw([builder.pop(false)?]),
            _ => Terminator::Goto([BlockId(blocks.get_start(block_index + 1))]),
        };
        for succ in term.succs() {
            if let Some(prev) = stack_on_entry.insert(*succ, builder.stack_top) {
                if prev != builder.stack_top {
                    return Err(Error::StackHeightMismatch);
                }
            }
        }
        builder.fill(term);
    }
    Ok(builder.fill())
}
