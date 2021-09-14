use std::collections::{BTreeMap, HashMap};
use ustr::Ustr;

use crate::ir::function::Function;
use crate::ir::module::{InitValue, NameAndType};

pub struct Method {
    pub is_private: bool,
    pub is_public: bool,
    pub is_static: bool,
}

pub struct Class {
    pub name: Ustr,
    pub superclass: Option<Ustr>,
    pub interfaces: Vec<Ustr>,
    pub static_fields: HashMap<NameAndType, InitValue>,
    pub instance_fields: Vec<NameAndType>,
    pub methods: HashMap<NameAndType, Method>,
    pub functions: HashMap<NameAndType, Function>,
}

pub type ClassMap = BTreeMap<Ustr, Class>;
