use std::collections::{BTreeSet, HashMap, HashSet};
use ustr::Ustr;

use crate::ir::function::{Function, Instruction, Value};
use crate::ir::module::{element_array_type, Member, Module, NameAndType};

#[derive(Default)]
struct Prototype {
    methods: BTreeSet<NameAndType>,
    constructed: bool,
    used: bool,
}

#[derive(Default)]
struct Reachability {
    function: HashSet<Member>,
    variable: HashSet<Member>,
    prototype: HashMap<Ustr, Prototype>,
    instance: HashSet<Ustr>,
    init: HashSet<Ustr>,
    order: Vec<Ustr>,
}

impl Reachability {
    fn new() -> Reachability {
        let mut reach = Reachability::default();
        reach.vpredefine("java/lang/String");
        reach.vpredefine("java/lang/Class");
        reach
    }

    fn vpredefine(&mut self, name: &str) {
        let proto = Prototype {
            methods: BTreeSet::new(),
            constructed: true,
            used: true,
        };
        self.prototype.insert(name.into(), proto);
    }

    fn vexports(&mut self, module: &Module) {
        for (class, pubname) in &module.exports {
            if let Some(proto) = module.prototypes.get(class) {
                self.vdefine(*class);
                self.vinit(module, *class);
                let mut hasinit = false;
                let wantinit = !pubname.is_empty();
                for name in proto.exports.keys() {
                    hasinit |= name.0 == "<init>";
                    if wantinit || name.0 != "<init>" {
                        self.vdirect(module, *class, *name);
                    }
                }
                if hasinit {
                    self.vconstruct(module, *class);
                }
            }
        }
    }

    fn vdefine(&mut self, class: Ustr) {
        let proto = self.prototype.entry(class).or_default();
        proto.used = true;
        if let Some(name) = element_array_type(&class) {
            self.vdefine(name);
        }
    }

    fn vinit(&mut self, module: &Module, class: Ustr) {
        if !self.init.insert(class) {
            return;
        }
        if class.starts_with("java/") || class.starts_with("org/apache/harmony/") {
            return;
        }
        let clinit = ("<clinit>".into(), "()V".into());
        self.vdirect(module, class, clinit);
        self.order.push(class);
    }

    fn vconstruct(&mut self, module: &Module, class: Ustr) {
        let proto = self.prototype.entry(class).or_default();
        if !proto.constructed {
            proto.constructed = true;
            proto.used = true;
            let methods = proto.methods.clone();
            for name in methods {
                self.vdirect(module, class, name);
            }
        }
    }

    fn vfield(&mut self, class: Ustr, name: NameAndType) {
        self.variable.insert(Member(class, name));
    }

    fn vdirect(&mut self, module: &Module, class: Ustr, name: NameAndType) {
        let member = module.resolve(class, name);
        if !self.function.insert(member) {
            return;
        }
        if let Some(func) = module.functions.get(&member) {
            self.vfunction(module, func);
        }
    }

    fn vvirtual(&mut self, module: &Module, class: Ustr, name: NameAndType) {
        if module.private(class, name) {
            self.vdirect(module, class, name);
            return;
        }
        for iclass in module.impls(class) {
            let proto = self.prototype.entry(*iclass).or_default();
            if !proto.methods.insert(name) {
                return;
            }
            if proto.constructed {
                self.vdirect(module, *iclass, name);
            }
        }
    }

    fn vfunction(&mut self, module: &Module, func: &Function) {
        func.for_each_instruction(|inst| match inst {
            Instruction::GetStatic(member) | Instruction::PutStatic(member, _) => {
                self.vinit(module, member.0);
                self.vfield(member.0, member.1);
            }
            Instruction::InvokeVirtual(member, _) => {
                self.vvirtual(module, member.0, member.1);
            }
            Instruction::InvokeDirect(member, _) => {
                self.vdirect(module, member.0, member.1);
            }
            Instruction::InvokeStatic(member, _) => {
                self.vinit(module, member.0);
                self.vdirect(module, member.0, member.1);
            }
            Instruction::New(class) => {
                self.vconstruct(module, *class);
            }
            Instruction::NewArray(class, _) => {
                self.vdefine(*class);
            }
            Instruction::InstanceOf(class, _) => {
                self.instance.insert(*class);
            }
            _ => {}
        });
        func.for_each_values(|values| {
            self.values(values);
        });
    }

    fn values(&mut self, values: &[Value]) {
        for value in values {
            if let Value::Class(name) = value {
                self.vdefine(*name);
            }
        }
    }

    fn prune(self, module: &mut Module) {
        let function = self.function;
        let variable = self.variable;
        let mut proto = self.prototype;
        proto.retain(|_, x| x.used);
        module.instances = self.instance;
        module.inits = self.order;
        module.functions.retain(|name, _| function.contains(name));
        module.variables.retain(|name, _| variable.contains(name));
        module.prototypes.retain(|name, _| proto.contains_key(name));
        for (name, reach) in proto {
            let data = module.prototypes.entry(name).or_default();
            module.superclass.entry(name).or_insert_with(|| vec![name]);
            if reach.constructed && !name.starts_with('[') {
                data.methods = reach.methods.into_iter().collect();
                data.constructed = true;
            }
        }
    }
}

pub fn apply(module: &mut Module) {
    let mut reach = Reachability::new();
    reach.vexports(module);
    reach.prune(module);
}

pub fn apply2(module: &mut Module) {
    let mut used = HashSet::new();
    for func in module.functions.values() {
        func.for_each_instruction(|inst| {
            if let Instruction::InvokeVirtual(member, _) = inst {
                for name in module.impls(member.0) {
                    used.insert(Member(*name, member.1));
                }
            }
        });
    }
    for (class, data) in &mut module.prototypes {
        data.methods.retain(|name| used.contains(&Member(*class, *name)));
    }
}
