use noak::reader::attributes::ArrayType;
use std::collections::{BTreeMap, HashMap};
use std::hash::BuildHasher;
use std::hash::Hash;
use ustr::Ustr;

use crate::ir::module::Member;

pub trait Push<K: Eq + Hash, V> {
    fn push(&mut self, key: K, value: V);
}

impl<K: Eq + Hash, V, S: BuildHasher> Push<K, V> for HashMap<K, Vec<V>, S> {
    fn push(&mut self, key: K, value: V) {
        self.entry(key).or_default().push(value);
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum NumType {
    Int,
    Long,
    Float,
    Double,
}

impl NumType {
    pub fn wide(self) -> bool {
        self == NumType::Long || self == NumType::Double
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ShiftOp {
    Shl,
    Shr,
    Ushr,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BitOp {
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ConvOp {
    I2B,
    I2C,
    I2S,
    I2L,
    L2I,
    L2F,
    L2D,
    D2I,
    D2F,
    D2L,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Pred {
    Eq,
    Ne,
    Lt,
    Ge,
    Gt,
    Le,
}

#[derive(Debug, Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BlockId(pub u32);

pub type InstId = u32;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Value {
    Undef,
    Null,
    Integer(i32),
    Long(i64),
    Float(u32),
    Double(u64),
    Class(Ustr),
    String(Ustr),
    SSARef(InstId),
}

pub type Values = Box<[Value]>;

#[derive(Debug)]
pub enum Instruction {
    ALoad(Option<ArrayType>, [Value; 2]),
    AStore(Option<ArrayType>, [Value; 3]),
    Arith(ArithOp, NumType, [Value; 2]),
    Negate(NumType, [Value; 1]),
    Shift(ShiftOp, bool, [Value; 2]),
    Bit(BitOp, bool, [Value; 2]),
    Conv(ConvOp, [Value; 1]),
    ICmp(Pred, [Value; 2]),
    FCmp(Pred, [Value; 2]),
    ACmp(bool, [Value; 2]),
    GetStatic(Member),
    PutStatic(Member, [Value; 1]),
    GetField(Member, [Value; 1]),
    PutField(Member, [Value; 2]),
    GetNativeVar(Ustr),
    GetNativeProp(Ustr, [Value; 1]),
    InvokeVirtual(Member, Values),
    InvokeDirect(Member, Values),
    InvokeStatic(Member, Values),
    InvokeNativeFunc(Ustr, bool, Values),
    InvokeNativeProp(Ustr, bool, Values),
    New(Ustr),
    NewArray(Ustr, Values),
    InstanceOf(Ustr, [Value; 1]),
    NativeToString([Value; 1]),
}

#[derive(Debug)]
pub enum Terminator {
    Return([Value; 1]),
    Throw([Value; 1]),
    Goto([BlockId; 1]),
    GotoIf([Value; 1], [BlockId; 2]),
    Switch([Value; 1], Box<[BlockId]>, Box<[i32]>),
}

#[derive(Debug)]
pub struct BasicBlock {
    pub preds: Vec<BlockId>,
    pub phis: HashMap<InstId, Values>,
    pub instructions: Vec<(InstId, Instruction)>,
    pub terminator: Terminator,
}

#[derive(Debug)]
pub struct Function {
    pub blocks: BTreeMap<BlockId, BasicBlock>,
    pub entry: BlockId,
    pub nextid: InstId,
    pub nargs: u32,
}

impl Value {
    pub fn replace_if(&mut self, iid: u32, value: Value) {
        if *self == Value::SSARef(iid) {
            *self = value;
        }
    }

    pub fn replace_if_mapped(&mut self, map: &HashMap<u32, Value>) {
        if let Value::SSARef(iid) = *self {
            if let Some(value) = map.get(&iid) {
                *self = *value;
            }
        }
    }
}

impl Instruction {
    pub fn is_empty(&self) -> bool {
        match self {
            Instruction::GetNativeVar(name) => name.is_empty(),
            _ => false,
        }
    }

    pub fn operands(&self) -> &[Value] {
        match self {
            Instruction::ALoad(_, oper) => oper,
            Instruction::AStore(_, oper) => oper,
            Instruction::Arith(_, _, oper) => oper,
            Instruction::Negate(_, oper) => oper,
            Instruction::Shift(_, _, oper) => oper,
            Instruction::Bit(_, _, oper) => oper,
            Instruction::Conv(_, oper) => oper,
            Instruction::ICmp(_, oper) => oper,
            Instruction::FCmp(_, oper) => oper,
            Instruction::ACmp(_, oper) => oper,
            Instruction::GetStatic(_) => &[],
            Instruction::PutStatic(_, oper) => oper,
            Instruction::GetField(_, oper) => oper,
            Instruction::PutField(_, oper) => oper,
            Instruction::GetNativeVar(_) => &[],
            Instruction::GetNativeProp(_, oper) => oper,
            Instruction::InvokeVirtual(_, oper) => oper,
            Instruction::InvokeDirect(_, oper) => oper,
            Instruction::InvokeStatic(_, oper) => oper,
            Instruction::InvokeNativeFunc(_, _, oper) => oper,
            Instruction::InvokeNativeProp(_, _, oper) => oper,
            Instruction::New(_) => &[],
            Instruction::NewArray(_, oper) => oper,
            Instruction::InstanceOf(_, oper) => oper,
            Instruction::NativeToString(oper) => oper,
        }
    }

    pub fn operands_mut(&mut self) -> &mut [Value] {
        match self {
            Instruction::ALoad(_, oper) => oper,
            Instruction::AStore(_, oper) => oper,
            Instruction::Arith(_, _, oper) => oper,
            Instruction::Negate(_, oper) => oper,
            Instruction::Shift(_, _, oper) => oper,
            Instruction::Bit(_, _, oper) => oper,
            Instruction::Conv(_, oper) => oper,
            Instruction::ICmp(_, oper) => oper,
            Instruction::FCmp(_, oper) => oper,
            Instruction::ACmp(_, oper) => oper,
            Instruction::GetStatic(_) => &mut [],
            Instruction::PutStatic(_, oper) => oper,
            Instruction::GetField(_, oper) => oper,
            Instruction::PutField(_, oper) => oper,
            Instruction::GetNativeVar(_) => &mut [],
            Instruction::GetNativeProp(_, oper) => oper,
            Instruction::InvokeVirtual(_, oper) => oper,
            Instruction::InvokeDirect(_, oper) => oper,
            Instruction::InvokeStatic(_, oper) => oper,
            Instruction::InvokeNativeFunc(_, _, oper) => oper,
            Instruction::InvokeNativeProp(_, _, oper) => oper,
            Instruction::New(_) => &mut [],
            Instruction::NewArray(_, oper) => oper,
            Instruction::InstanceOf(_, oper) => oper,
            Instruction::NativeToString(oper) => oper,
        }
    }
}

impl Terminator {
    pub fn operands(&self) -> &[Value] {
        match self {
            Terminator::Return(oper) => oper,
            Terminator::Throw(oper) => oper,
            Terminator::Goto(_) => &[],
            Terminator::GotoIf(oper, _) => oper,
            Terminator::Switch(oper, _, _) => oper,
        }
    }

    pub fn operands_mut(&mut self) -> &mut [Value] {
        match self {
            Terminator::Return(oper) => oper,
            Terminator::Throw(oper) => oper,
            Terminator::Goto(_) => &mut [],
            Terminator::GotoIf(oper, _) => oper,
            Terminator::Switch(oper, _, _) => oper,
        }
    }

    pub fn succs(&self) -> &[BlockId] {
        match self {
            Terminator::Return(_) | Terminator::Throw(_) => &[],
            Terminator::Goto(dest) => dest,
            Terminator::GotoIf(_, dest) => dest,
            Terminator::Switch(_, dest, _) => dest,
        }
    }
}

impl BasicBlock {
    pub fn new(terminator: Terminator) -> BasicBlock {
        BasicBlock {
            preds: Vec::new(),
            phis: HashMap::new(),
            instructions: Vec::new(),
            terminator,
        }
    }

    pub fn has_instruction(&self, iid: u32) -> bool {
        self.instructions.iter().any(|x| x.0 == iid)
    }
}

impl Function {
    pub fn new(nargs: u32) -> Function {
        let mut blocks = BTreeMap::new();
        blocks.insert(BlockId(!0), BasicBlock::new(Terminator::Goto([BlockId(0)])));
        Function {
            blocks,
            entry: BlockId(!0),
            nextid: nargs,
            nargs,
        }
    }

    pub fn nextid(&mut self) -> InstId {
        let iid = self.nextid;
        self.nextid += 1;
        iid
    }

    pub fn for_each_instruction<F: FnMut(&Instruction)>(&self, mut f: F) {
        for block in self.blocks.values() {
            for (_, inst) in &block.instructions {
                f(inst);
            }
        }
    }

    pub fn for_each_instruction_mut<F: FnMut(&mut Instruction)>(&mut self, mut f: F) {
        for block in self.blocks.values_mut() {
            for (_, inst) in &mut block.instructions {
                f(inst);
            }
            block.instructions.retain(|inst| !inst.1.is_empty());
        }
    }

    pub fn for_each_values<F: FnMut(&[Value])>(&self, mut f: F) {
        for block in self.blocks.values() {
            for (_, inst) in &block.instructions {
                f(inst.operands());
            }
            for values in block.phis.values() {
                f(values);
            }
            f(block.terminator.operands());
        }
    }

    pub fn succs(&self) -> Vec<(BlockId, Vec<BlockId>)> {
        let mut succs = Vec::new();
        for (id, block) in &self.blocks {
            succs.push((*id, block.terminator.succs().into()));
        }
        succs.sort();
        succs
    }
}
