use serde::de::{Deserialize, Deserializer, Error};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use ustr::Ustr;

use crate::ir::class::{Class, ClassMap};
use crate::ir::function::{Function, Push};
use crate::ir::natives::NativeMap;

pub type NameAndType = (Ustr, Ustr);

#[derive(Debug, Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Member(pub Ustr, pub NameAndType);

pub enum InitValue {
    Null,
    Number(f64),
    Long(i64),
    String(Ustr),
}

#[derive(Default)]
pub struct Prototype {
    pub fields: Vec<NameAndType>,
    pub methods: BTreeSet<NameAndType>,
    pub exports: BTreeMap<NameAndType, bool>,
    pub constructed: bool,
}

pub type ExportMap = BTreeMap<Ustr, String>;

pub struct Module {
    pub natives: NativeMap,
    pub exports: ExportMap,
    pub superclass: HashMap<Ustr, Vec<Ustr>>,
    pub implementations: HashMap<Ustr, Vec<Ustr>>,
    pub resolved: HashMap<Member, (Ustr, bool)>,
    pub functions: BTreeMap<Member, Function>,
    pub variables: BTreeMap<Member, InitValue>,
    pub prototypes: BTreeMap<Ustr, Prototype>,
    pub instances: HashSet<Ustr>,
    pub inits: Vec<Ustr>,
}

impl Member {
    pub fn parse(value: &str) -> Option<Member> {
        let (class, nametype) = value.rsplit_once('.')?;
        let (name, desc) = nametype.rsplit_once(':')?;
        Some(Member(class.to_string().replace('.', "/").into(), (name.into(), desc.into())))
    }
}

impl<'de> Deserialize<'de> for Member {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = Deserialize::deserialize(deserializer)?;
        match Member::parse(value) {
            Some(parsed) => Ok(parsed),
            None => Err(D::Error::custom("malformed")),
        }
    }
}

fn flatten(classes: &ClassMap, class: Ustr, seen: &mut HashSet<Ustr>, into: &mut Vec<Ustr>) {
    if !seen.insert(class) {
        return;
    }
    into.push(class);
    if let Some(data) = classes.get(&class) {
        if let Some(name) = data.superclass {
            flatten(classes, name, seen, into);
        }
        for name in &data.interfaces {
            flatten(classes, *name, seen, into);
        }
    }
}

fn collect(class: Ustr, data: &Class, resolved: &mut HashMap<Member, (Ustr, bool)>) {
    for (name, method) in &data.methods {
        if class == data.name || !method.is_private {
            let member = Member(class, *name);
            let value = (data.name, method.is_private);
            resolved.entry(member).or_insert(value);
        }
    }
}

impl Module {
    pub fn new(classes: ClassMap, natives: NativeMap, exports: ExportMap) -> Module {
        let mut superclass = HashMap::default();
        let mut implementations = HashMap::default();
        let mut resolved = HashMap::new();
        let mut functions = BTreeMap::new();
        let mut variables = BTreeMap::new();
        let mut prototypes = BTreeMap::default();
        for (class, data) in &classes {
            let mut seen = HashSet::new();
            let mut supers = Vec::new();
            let mut proto = Prototype::default();
            flatten(&classes, *class, &mut seen, &mut supers);
            for name in &supers {
                if let Some(data) = classes.get(name) {
                    proto.fields.extend(&data.instance_fields);
                    collect(*class, data, &mut resolved);
                }
                implementations.push(*name, *class);
            }
            if exports.contains_key(class) {
                for (name, method) in &data.methods {
                    if method.is_public {
                        proto.exports.insert(*name, method.is_static);
                    }
                }
            }
            superclass.insert(*class, supers);
            prototypes.insert(*class, proto);
        }
        for (class, data) in classes {
            for (name, func) in data.functions {
                let member = Member(class, name);
                if !natives.contains_key(&member) {
                    functions.insert(member, func);
                }
            }
            for (name, init) in data.static_fields {
                variables.insert(Member(class, name), init);
            }
        }
        Module {
            natives,
            exports,
            superclass,
            implementations,
            resolved,
            functions,
            variables,
            prototypes,
            instances: HashSet::new(),
            inits: Vec::new(),
        }
    }

    pub fn impls(&self, class: Ustr) -> &[Ustr] {
        match self.implementations.get(&class) {
            Some(list) => list,
            None => &[],
        }
    }

    pub fn private(&self, class: Ustr, name: NameAndType) -> bool {
        match self.resolved.get(&Member(class, name)) {
            Some((_, private)) => *private,
            None => false,
        }
    }

    pub fn resolve(&self, mut class: Ustr, name: NameAndType) -> Member {
        if class.starts_with('[') {
            class = "[Ljava/lang/Object;".into();
        }
        let mut member = Member(class, name);
        if let Some((class, _)) = self.resolved.get(&member) {
            member.0 = *class;
        }
        member
    }
}

pub fn element_array_type(class: &str) -> Option<Ustr> {
    if class.starts_with("[[") {
        return Some(class[1..].into());
    }
    None
}
