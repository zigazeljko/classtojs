use noak::error::DecodeError;
use noak::reader::attributes::{Attribute, AttributeContent, Code};
use noak::reader::cpool::{ConstantPool, Item, ToValue};
use noak::reader::Class;
use noak::AccessFlags;
use std::collections::HashMap;
use ustr::Ustr;

use crate::ir::class::{Class as IrClass, Method};
use crate::ir::function::Function;
use crate::ir::module::InitValue;

mod code;
mod flow;
mod ssa;
pub mod types;

fn as_code<'a>(attr: Attribute<'a>, pool: &ConstantPool<'a>) -> Option<Code<'a>> {
    if let Ok(AttributeContent::Code(data)) = attr.read_content(pool) {
        return Some(data);
    }
    None
}

fn as_init<'a>(attr: Attribute<'a>, pool: &'a ConstantPool<'a>) -> Option<&'a Item<'a>> {
    if let Ok(AttributeContent::ConstantValue(value)) = attr.read_content(pool) {
        return pool.get(value.value()).ok();
    }
    None
}

fn initvalue<'a>(value: Option<&'a Item<'a>>, pool: &ConstantPool<'a>, desc: Ustr) -> Result<InitValue, DecodeError> {
    Ok(match value {
        Some(Item::Integer(val)) => InitValue::Number(val.value as f64),
        Some(Item::Long(val)) => InitValue::Long(val.value),
        Some(Item::Float(val)) => InitValue::Number(val.value as f64),
        Some(Item::Double(val)) => InitValue::Number(val.value),
        Some(Item::String(val)) => InitValue::String(code::ustr(pool.get(val.string)?.content)),
        _ => match desc.as_str() {
            "B" | "C" | "D" | "F" | "I" | "S" | "Z" => InitValue::Number(0.0),
            "J" => InitValue::Long(0),
            _ => InitValue::Null,
        },
    })
}

fn function<'a>(data: Code<'a>, pool: &ConstantPool<'a>, desc: Ustr, is_static: bool) -> Option<Function> {
    let insts = code::parse_code(&data, pool).ok()?;
    let (blocks, succs) = flow::split_basic_blocks(&insts);
    let (order, preds) = flow::sort_reverse_postorder(succs)?;
    let func = ssa::build(&blocks, &order, &preds, desc, is_static).ok()?;
    Some(func)
}

pub fn parse(buf: &[u8]) -> Result<(Ustr, Class), DecodeError> {
    let mut class = Class::new(buf)?;
    let name = code::ustr(class.this_class_name()?);
    Ok((name, class))
}

pub fn transform(mut class: Class) -> Result<IrClass, DecodeError> {
    let name = code::ustr(class.this_class_name()?);
    let superclass = class.super_class_name()?.map(code::ustr);
    let mut interfaces = Vec::new();
    let mut static_fields = HashMap::new();
    let mut instance_fields = Vec::new();
    let mut methods = HashMap::new();
    let mut functions = HashMap::new();
    for iface in class.interfaces()? {
        let iface = iface?;
        let pool = class.pool()?;
        let iid = code::ustr(pool.get(pool.get(iface)?.name)?.content);
        interfaces.push(iid);
    }
    for field in class.fields()? {
        let field = field?;
        let pool = class.pool()?;
        let name = code::ustr(field.name().retrieve_from(pool)?);
        let desc = code::ustr(field.descriptor().retrieve_from(pool)?);
        let flags = field.access_flags();
        if flags.contains(AccessFlags::STATIC) {
            let value = field.attributes().find_map(|attr| as_init(attr.ok()?, pool));
            static_fields.insert((name, desc), initvalue(value, pool, desc)?);
        } else {
            instance_fields.push((name, desc));
        }
    }
    for method in class.methods()? {
        let method = method?;
        let pool = class.pool()?;
        let name = code::ustr(method.name().retrieve_from(pool)?);
        let desc = code::ustr(method.descriptor().retrieve_from(pool)?);
        let flags = method.access_flags();
        let irmethod = Method {
            is_private: flags.contains(AccessFlags::PRIVATE),
            is_public: flags.contains(AccessFlags::PUBLIC),
            is_static: flags.contains(AccessFlags::STATIC),
        };
        methods.insert((name, desc), irmethod);
        if let Some(data) = method.attributes().find_map(|attr| as_code(attr.ok()?, pool)) {
            if let Some(func) = function(data, pool, desc, flags.contains(AccessFlags::STATIC)) {
                functions.insert((name, desc), func);
            }
        }
    }
    Ok(IrClass {
        name,
        superclass,
        interfaces,
        static_fields,
        instance_fields,
        methods,
        functions,
    })
}
