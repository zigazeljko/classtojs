use noak::error::DecodeError;
use noak::mutf8::MStr;
use noak::reader::attributes::{ArrayType, Code, RawInstruction as RI, RawInstructions};
use noak::reader::cpool::{ConstantPool, InterfaceMethodRef, Item, MethodRef, ToValue};
use ustr::Ustr;

use crate::ir::function::{ArithOp, BitOp, NumType, Pred, ShiftOp};
use crate::ir::module::NameAndType;

pub enum TruncType {
    Byte,
    Char,
    Short,
}

pub enum CmpType {
    ICmp0,
    ICmp,
    LCmp,
    FCmpL,
    FCmpG,
    DCmpL,
    DCmpG,
    ACmp,
    ACmp0,
}

pub enum MonitorOp {
    Enter,
    Exit,
}

pub enum Inst {
    Nop,
    AConstNull,
    IConst(i32),
    LConst(i64),
    FConst(f32),
    DConst(f64),
    CConst(Ustr),
    SConst(Ustr),
    Load(u16, bool),
    Store(u16, bool),
    ALoad(Option<ArrayType>),
    AStore(Option<ArrayType>),
    Pop(bool),
    Dup(bool),
    DupX1(bool),
    DupX2(bool),
    Swap,
    Arith(ArithOp, NumType),
    Neg(NumType),
    IInc(u16, i16),
    Shift(ShiftOp, bool),
    Bit(BitOp, bool),
    Conv(NumType, NumType),
    Trunc(TruncType),
    GotoIf(Pred, CmpType, u32),
    Goto(u32),
    Jsr(u32),
    Ret(u16),
    Switch(u32, Box<[(i32, u32)]>),
    Return(Option<bool>),
    GetStatic(Ustr, NameAndType),
    PutStatic(Ustr, NameAndType),
    GetField(Ustr, NameAndType),
    PutField(Ustr, NameAndType),
    InvokeVirtual(Ustr, NameAndType),
    InvokeSpecial(Ustr, NameAndType),
    InvokeStatic(Ustr, NameAndType),
    InvokeDynamic(u16, NameAndType),
    New(Ustr),
    NewArray(ArrayType),
    ANewArray(Ustr),
    ArrayLength,
    AThrow,
    CheckCast(Ustr),
    InstanceOf(Ustr),
    Monitor(MonitorOp),
    MultiANewArray(Ustr, u8),
}

pub enum ParseError {
    Decode(DecodeError),
    UnexpectedEoi,
    BadConstantRef,
    UnexpectedInstruction,
}

impl From<DecodeError> for ParseError {
    fn from(err: DecodeError) -> Self {
        ParseError::Decode(err)
    }
}

fn get_br(pos: u32, offset: i16) -> u32 {
    pos.wrapping_add(offset as u32)
}

fn get_br32(pos: u32, offset: i32) -> u32 {
    pos.wrapping_add(offset as u32)
}

fn get_cmp_if(iter: &mut RawInstructions, typ: CmpType) -> Result<Inst, ParseError> {
    match iter.next() {
        Some(Ok((ipos, inst))) => match (ipos.as_u32(), inst) {
            (pos, RI::IfEq { offset }) => Ok(Inst::GotoIf(Pred::Eq, typ, get_br(pos, offset))),
            (pos, RI::IfNe { offset }) => Ok(Inst::GotoIf(Pred::Ne, typ, get_br(pos, offset))),
            (pos, RI::IfLt { offset }) => Ok(Inst::GotoIf(Pred::Lt, typ, get_br(pos, offset))),
            (pos, RI::IfGe { offset }) => Ok(Inst::GotoIf(Pred::Ge, typ, get_br(pos, offset))),
            (pos, RI::IfGt { offset }) => Ok(Inst::GotoIf(Pred::Gt, typ, get_br(pos, offset))),
            (pos, RI::IfLe { offset }) => Ok(Inst::GotoIf(Pred::Le, typ, get_br(pos, offset))),
            _ => Err(ParseError::UnexpectedInstruction),
        },
        Some(Err(err)) => Err(ParseError::Decode(err)),
        None => Err(ParseError::UnexpectedEoi),
    }
}

pub fn ustr(data: &MStr) -> Ustr {
    (unsafe { std::str::from_utf8_unchecked(data.as_bytes()) }).into()
}

pub fn parse_code<'a>(code: &Code<'a>, pool: &ConstantPool<'a>) -> Result<Vec<(u32, Inst)>, ParseError> {
    let mut out = Vec::new();
    let mut iter = code.raw_instructions();
    let mut max_pos = 0;
    while let Some(item) = iter.next() {
        let (ipos, rinst) = item?;
        let pos = ipos.as_u32();
        let inst = match rinst {
            RI::Nop => Inst::Nop,
            RI::AConstNull => Inst::AConstNull,
            RI::IConstM1 => Inst::IConst(-1),
            RI::IConst0 => Inst::IConst(0),
            RI::IConst1 => Inst::IConst(1),
            RI::IConst2 => Inst::IConst(2),
            RI::IConst3 => Inst::IConst(3),
            RI::IConst4 => Inst::IConst(4),
            RI::IConst5 => Inst::IConst(5),
            RI::LConst0 => Inst::LConst(0),
            RI::LConst1 => Inst::LConst(1),
            RI::FConst0 => Inst::FConst(0.0),
            RI::FConst1 => Inst::FConst(1.0),
            RI::FConst2 => Inst::FConst(2.0),
            RI::DConst0 => Inst::DConst(0.0),
            RI::DConst1 => Inst::DConst(1.0),
            RI::BIPush { value } => Inst::IConst(value as i32),
            RI::SIPush { value } => Inst::IConst(value as i32),
            RI::LdC { index } | RI::LdCW { index } => match pool.get(index)? {
                Item::Integer(val) => Inst::IConst(val.value),
                Item::Float(val) => Inst::FConst(val.value),
                Item::Class(val) => Inst::CConst(ustr(pool.get(val.name)?.content)),
                Item::String(val) => Inst::SConst(ustr(pool.get(val.string)?.content)),
                _ => return Err(ParseError::BadConstantRef),
            },
            RI::LdC2W { index } => match pool.get(index)? {
                Item::Long(val) => Inst::LConst(val.value),
                Item::Double(val) => Inst::DConst(val.value),
                _ => return Err(ParseError::BadConstantRef),
            },
            RI::ILoad { index } => Inst::Load(index as u16, false),
            RI::ILoadW { index } => Inst::Load(index, false),
            RI::LLoad { index } => Inst::Load(index as u16, true),
            RI::LLoadW { index } => Inst::Load(index, true),
            RI::FLoad { index } => Inst::Load(index as u16, false),
            RI::FLoadW { index } => Inst::Load(index, false),
            RI::DLoad { index } => Inst::Load(index as u16, true),
            RI::DLoadW { index } => Inst::Load(index, true),
            RI::ALoad { index } => Inst::Load(index as u16, false),
            RI::ALoadW { index } => Inst::Load(index, false),
            RI::ILoad0 => Inst::Load(0, false),
            RI::ILoad1 => Inst::Load(1, false),
            RI::ILoad2 => Inst::Load(2, false),
            RI::ILoad3 => Inst::Load(3, false),
            RI::LLoad0 => Inst::Load(0, true),
            RI::LLoad1 => Inst::Load(1, true),
            RI::LLoad2 => Inst::Load(2, true),
            RI::LLoad3 => Inst::Load(3, true),
            RI::FLoad0 => Inst::Load(0, false),
            RI::FLoad1 => Inst::Load(1, false),
            RI::FLoad2 => Inst::Load(2, false),
            RI::FLoad3 => Inst::Load(3, false),
            RI::DLoad0 => Inst::Load(0, true),
            RI::DLoad1 => Inst::Load(1, true),
            RI::DLoad2 => Inst::Load(2, true),
            RI::DLoad3 => Inst::Load(3, true),
            RI::ALoad0 => Inst::Load(0, false),
            RI::ALoad1 => Inst::Load(1, false),
            RI::ALoad2 => Inst::Load(2, false),
            RI::ALoad3 => Inst::Load(3, false),
            RI::IALoad => Inst::ALoad(Some(ArrayType::Int)),
            RI::LALoad => Inst::ALoad(Some(ArrayType::Long)),
            RI::FALoad => Inst::ALoad(Some(ArrayType::Float)),
            RI::DALoad => Inst::ALoad(Some(ArrayType::Double)),
            RI::AALoad => Inst::ALoad(None),
            RI::BALoad => Inst::ALoad(Some(ArrayType::Byte)),
            RI::CALoad => Inst::ALoad(Some(ArrayType::Char)),
            RI::SALoad => Inst::ALoad(Some(ArrayType::Short)),
            RI::IStore { index } => Inst::Store(index as u16, false),
            RI::IStoreW { index } => Inst::Store(index, false),
            RI::LStore { index } => Inst::Store(index as u16, true),
            RI::LStoreW { index } => Inst::Store(index, true),
            RI::FStore { index } => Inst::Store(index as u16, false),
            RI::FStoreW { index } => Inst::Store(index, false),
            RI::DStore { index } => Inst::Store(index as u16, true),
            RI::DStoreW { index } => Inst::Store(index, true),
            RI::AStore { index } => Inst::Store(index as u16, false),
            RI::AStoreW { index } => Inst::Store(index, false),
            RI::IStore0 => Inst::Store(0, false),
            RI::IStore1 => Inst::Store(1, false),
            RI::IStore2 => Inst::Store(2, false),
            RI::IStore3 => Inst::Store(3, false),
            RI::LStore0 => Inst::Store(0, true),
            RI::LStore1 => Inst::Store(1, true),
            RI::LStore2 => Inst::Store(2, true),
            RI::LStore3 => Inst::Store(3, true),
            RI::FStore0 => Inst::Store(0, false),
            RI::FStore1 => Inst::Store(1, false),
            RI::FStore2 => Inst::Store(2, false),
            RI::FStore3 => Inst::Store(3, false),
            RI::DStore0 => Inst::Store(0, true),
            RI::DStore1 => Inst::Store(1, true),
            RI::DStore2 => Inst::Store(2, true),
            RI::DStore3 => Inst::Store(3, true),
            RI::AStore0 => Inst::Store(0, false),
            RI::AStore1 => Inst::Store(1, false),
            RI::AStore2 => Inst::Store(2, false),
            RI::AStore3 => Inst::Store(3, false),
            RI::IAStore => Inst::AStore(Some(ArrayType::Int)),
            RI::LAStore => Inst::AStore(Some(ArrayType::Long)),
            RI::FAStore => Inst::AStore(Some(ArrayType::Float)),
            RI::DAStore => Inst::AStore(Some(ArrayType::Double)),
            RI::AAStore => Inst::AStore(None),
            RI::BAStore => Inst::AStore(Some(ArrayType::Byte)),
            RI::CAStore => Inst::AStore(Some(ArrayType::Char)),
            RI::SAStore => Inst::AStore(Some(ArrayType::Short)),
            RI::Pop => Inst::Pop(false),
            RI::Pop2 => Inst::Pop(true),
            RI::Dup => Inst::Dup(false),
            RI::DupX1 => Inst::DupX1(false),
            RI::DupX2 => Inst::DupX2(false),
            RI::Dup2 => Inst::Dup(true),
            RI::Dup2X1 => Inst::DupX1(true),
            RI::Dup2X2 => Inst::DupX2(true),
            RI::Swap => Inst::Swap,
            RI::IAdd => Inst::Arith(ArithOp::Add, NumType::Int),
            RI::LAdd => Inst::Arith(ArithOp::Add, NumType::Long),
            RI::FAdd => Inst::Arith(ArithOp::Add, NumType::Float),
            RI::DAdd => Inst::Arith(ArithOp::Add, NumType::Double),
            RI::ISub => Inst::Arith(ArithOp::Sub, NumType::Int),
            RI::LSub => Inst::Arith(ArithOp::Sub, NumType::Long),
            RI::FSub => Inst::Arith(ArithOp::Sub, NumType::Float),
            RI::DSub => Inst::Arith(ArithOp::Sub, NumType::Double),
            RI::IMul => Inst::Arith(ArithOp::Mul, NumType::Int),
            RI::LMul => Inst::Arith(ArithOp::Mul, NumType::Long),
            RI::FMul => Inst::Arith(ArithOp::Mul, NumType::Float),
            RI::DMul => Inst::Arith(ArithOp::Mul, NumType::Double),
            RI::IDiv => Inst::Arith(ArithOp::Div, NumType::Int),
            RI::LDiv => Inst::Arith(ArithOp::Div, NumType::Long),
            RI::FDiv => Inst::Arith(ArithOp::Div, NumType::Float),
            RI::DDiv => Inst::Arith(ArithOp::Div, NumType::Double),
            RI::IRem => Inst::Arith(ArithOp::Rem, NumType::Int),
            RI::LRem => Inst::Arith(ArithOp::Rem, NumType::Long),
            RI::FRem => Inst::Arith(ArithOp::Rem, NumType::Float),
            RI::DRem => Inst::Arith(ArithOp::Rem, NumType::Double),
            RI::INeg => Inst::Neg(NumType::Int),
            RI::LNeg => Inst::Neg(NumType::Long),
            RI::FNeg => Inst::Neg(NumType::Float),
            RI::DNeg => Inst::Neg(NumType::Double),
            RI::IShL => Inst::Shift(ShiftOp::Shl, false),
            RI::LShL => Inst::Shift(ShiftOp::Shl, true),
            RI::IShR => Inst::Shift(ShiftOp::Shr, false),
            RI::LShR => Inst::Shift(ShiftOp::Shr, true),
            RI::IUShR => Inst::Shift(ShiftOp::Ushr, false),
            RI::LUShR => Inst::Shift(ShiftOp::Ushr, true),
            RI::IAnd => Inst::Bit(BitOp::And, false),
            RI::LAnd => Inst::Bit(BitOp::And, true),
            RI::IOr => Inst::Bit(BitOp::Or, false),
            RI::LOr => Inst::Bit(BitOp::Or, true),
            RI::IXor => Inst::Bit(BitOp::Xor, false),
            RI::LXor => Inst::Bit(BitOp::Xor, true),
            RI::IInc { index, value } => Inst::IInc(index as u16, value as i16),
            RI::IIncW { index, value } => Inst::IInc(index, value),
            RI::I2L => Inst::Conv(NumType::Int, NumType::Long),
            RI::I2F => Inst::Conv(NumType::Int, NumType::Float),
            RI::I2D => Inst::Conv(NumType::Int, NumType::Double),
            RI::L2I => Inst::Conv(NumType::Long, NumType::Int),
            RI::L2F => Inst::Conv(NumType::Long, NumType::Float),
            RI::L2D => Inst::Conv(NumType::Long, NumType::Double),
            RI::F2I => Inst::Conv(NumType::Float, NumType::Int),
            RI::F2L => Inst::Conv(NumType::Float, NumType::Long),
            RI::F2D => Inst::Conv(NumType::Float, NumType::Double),
            RI::D2I => Inst::Conv(NumType::Double, NumType::Int),
            RI::D2L => Inst::Conv(NumType::Double, NumType::Long),
            RI::D2F => Inst::Conv(NumType::Double, NumType::Float),
            RI::I2B => Inst::Trunc(TruncType::Byte),
            RI::I2C => Inst::Trunc(TruncType::Char),
            RI::I2S => Inst::Trunc(TruncType::Short),
            RI::LCmp => get_cmp_if(&mut iter, CmpType::LCmp)?,
            RI::FCmpL => get_cmp_if(&mut iter, CmpType::FCmpL)?,
            RI::FCmpG => get_cmp_if(&mut iter, CmpType::FCmpG)?,
            RI::DCmpL => get_cmp_if(&mut iter, CmpType::DCmpL)?,
            RI::DCmpG => get_cmp_if(&mut iter, CmpType::DCmpG)?,
            RI::IfEq { offset } => Inst::GotoIf(Pred::Eq, CmpType::ICmp0, get_br(pos, offset)),
            RI::IfNe { offset } => Inst::GotoIf(Pred::Ne, CmpType::ICmp0, get_br(pos, offset)),
            RI::IfLt { offset } => Inst::GotoIf(Pred::Lt, CmpType::ICmp0, get_br(pos, offset)),
            RI::IfGe { offset } => Inst::GotoIf(Pred::Ge, CmpType::ICmp0, get_br(pos, offset)),
            RI::IfGt { offset } => Inst::GotoIf(Pred::Gt, CmpType::ICmp0, get_br(pos, offset)),
            RI::IfLe { offset } => Inst::GotoIf(Pred::Le, CmpType::ICmp0, get_br(pos, offset)),
            RI::IfICmpEq { offset } => Inst::GotoIf(Pred::Eq, CmpType::ICmp, get_br(pos, offset)),
            RI::IfICmpNe { offset } => Inst::GotoIf(Pred::Ne, CmpType::ICmp, get_br(pos, offset)),
            RI::IfICmpLt { offset } => Inst::GotoIf(Pred::Lt, CmpType::ICmp, get_br(pos, offset)),
            RI::IfICmpGe { offset } => Inst::GotoIf(Pred::Ge, CmpType::ICmp, get_br(pos, offset)),
            RI::IfICmpGt { offset } => Inst::GotoIf(Pred::Gt, CmpType::ICmp, get_br(pos, offset)),
            RI::IfICmpLe { offset } => Inst::GotoIf(Pred::Le, CmpType::ICmp, get_br(pos, offset)),
            RI::IfACmpEq { offset } => Inst::GotoIf(Pred::Eq, CmpType::ACmp, get_br(pos, offset)),
            RI::IfACmpNe { offset } => Inst::GotoIf(Pred::Ne, CmpType::ACmp, get_br(pos, offset)),
            RI::Goto { offset } => Inst::Goto(get_br(pos, offset)),
            RI::JSr { offset } => Inst::Jsr(get_br(pos, offset)),
            RI::Ret { index } => Inst::Ret(index as u16),
            RI::RetW { index } => Inst::Ret(index),
            RI::TableSwitch(table) => Inst::Switch(
                get_br32(pos, table.default_offset()),
                table.pairs().map(|pair| (pair.key(), get_br32(pos, pair.offset()))).collect(),
            ),
            RI::LookupSwitch(lookup) => Inst::Switch(
                get_br32(pos, lookup.default_offset()),
                lookup.pairs().map(|pair| (pair.key(), get_br32(pos, pair.offset()))).collect(),
            ),
            RI::IReturn => Inst::Return(Some(false)),
            RI::LReturn => Inst::Return(Some(true)),
            RI::FReturn => Inst::Return(Some(false)),
            RI::DReturn => Inst::Return(Some(true)),
            RI::AReturn => Inst::Return(Some(false)),
            RI::Return => Inst::Return(None),
            RI::GetStatic { index } => {
                let field = index.retrieve_from(pool)?;
                Inst::GetStatic(
                    ustr(field.class.name),
                    (ustr(field.name_and_type.name), ustr(field.name_and_type.descriptor)),
                )
            }
            RI::PutStatic { index } => {
                let field = index.retrieve_from(pool)?;
                Inst::PutStatic(
                    ustr(field.class.name),
                    (ustr(field.name_and_type.name), ustr(field.name_and_type.descriptor)),
                )
            }
            RI::GetField { index } => {
                let field = index.retrieve_from(pool)?;
                Inst::GetField(
                    ustr(field.class.name),
                    (ustr(field.name_and_type.name), ustr(field.name_and_type.descriptor)),
                )
            }
            RI::PutField { index } => {
                let field = index.retrieve_from(pool)?;
                Inst::PutField(
                    ustr(field.class.name),
                    (ustr(field.name_and_type.name), ustr(field.name_and_type.descriptor)),
                )
            }
            RI::InvokeVirtual { index } => {
                let method = index.retrieve_from(pool)?;
                Inst::InvokeVirtual(
                    ustr(method.class.name),
                    (ustr(method.name_and_type.name), ustr(method.name_and_type.descriptor)),
                )
            }
            RI::InvokeSpecial { index } => match pool.get(index)? {
                Item::MethodRef(MethodRef { class, name_and_type }) | Item::InterfaceMethodRef(InterfaceMethodRef { class, name_and_type }) => {
                    let nametype = pool.get(*name_and_type)?;
                    Inst::InvokeSpecial(
                        ustr(pool.get(pool.get(*class)?.name)?.content),
                        (ustr(pool.get(nametype.name)?.content), ustr(pool.get(nametype.descriptor)?.content)),
                    )
                }
                _ => return Err(ParseError::BadConstantRef),
            },
            RI::InvokeStatic { index } => match pool.get(index)? {
                Item::MethodRef(MethodRef { class, name_and_type }) | Item::InterfaceMethodRef(InterfaceMethodRef { class, name_and_type }) => {
                    let nametype = pool.get(*name_and_type)?;
                    Inst::InvokeStatic(
                        ustr(pool.get(pool.get(*class)?.name)?.content),
                        (ustr(pool.get(nametype.name)?.content), ustr(pool.get(nametype.descriptor)?.content)),
                    )
                }
                _ => return Err(ParseError::BadConstantRef),
            },
            RI::InvokeInterface { index, .. } => {
                let method = index.retrieve_from(pool)?;
                Inst::InvokeVirtual(
                    ustr(method.class.name),
                    (ustr(method.name_and_type.name), ustr(method.name_and_type.descriptor)),
                )
            }
            RI::InvokeDynamic { index } => {
                let dynamic = index.retrieve_from(pool)?;
                Inst::InvokeDynamic(
                    dynamic.bootstrap_method_attr,
                    (ustr(dynamic.name_and_type.name), ustr(dynamic.name_and_type.descriptor)),
                )
            }
            RI::New { index } => Inst::New(ustr(index.retrieve_from(pool)?.name)),
            RI::NewArray { atype } => Inst::NewArray(atype),
            RI::ANewArray { index } => Inst::ANewArray(ustr(index.retrieve_from(pool)?.name)),
            RI::ArrayLength => Inst::ArrayLength,
            RI::AThrow => Inst::AThrow,
            RI::CheckCast { index } => Inst::CheckCast(ustr(index.retrieve_from(pool)?.name)),
            RI::InstanceOf { index } => Inst::InstanceOf(ustr(index.retrieve_from(pool)?.name)),
            RI::MonitorEnter => Inst::Monitor(MonitorOp::Enter),
            RI::MonitorExit => Inst::Monitor(MonitorOp::Exit),
            RI::MultiANewArray { index, dimensions } => Inst::MultiANewArray(ustr(index.retrieve_from(pool)?.name), dimensions),
            RI::IfNull { offset } => Inst::GotoIf(Pred::Eq, CmpType::ACmp0, get_br(pos, offset)),
            RI::IfNonNull { offset } => Inst::GotoIf(Pred::Ne, CmpType::ACmp0, get_br(pos, offset)),
            RI::GotoW { offset } => Inst::Goto(get_br32(pos, offset)),
            RI::JSrW { offset } => Inst::Jsr(get_br32(pos, offset)),
        };
        out.push((pos, inst));
        max_pos = pos + 1;
    }
    out.push((max_pos, Inst::Return(None)));
    Ok(out)
}
