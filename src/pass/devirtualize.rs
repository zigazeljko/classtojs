use std::collections::HashSet;
use std::mem;

use crate::ir::function::Instruction;
use crate::ir::module::{Member, Module};
use crate::ir::natives;

pub fn apply(module: &mut Module) {
    let mut needed = HashSet::new();
    for (class, data) in &module.prototypes {
        for name in &data.methods {
            let resolved = module.resolve(*class, *name);
            for superclass in &module.superclass[class] {
                if let Some(original) = module.resolved.get(&Member(*superclass, *name)) {
                    if original.0 != resolved.0 && !original.1 {
                        needed.insert(Member(*superclass, *name));
                    }
                }
            }
        }
    }
    let mut functions = mem::take(&mut module.functions);
    for func in functions.values_mut() {
        func.for_each_instruction_mut(|inst| match inst {
            Instruction::InvokeVirtual(member, values) => {
                if !needed.contains(member) {
                    *member = module.resolve(member.0, member.1);
                    *inst = Instruction::InvokeDirect(*member, mem::take(values));
                    natives::replace(&module.natives, inst);
                }
            }
            Instruction::InvokeDirect(member, _) => {
                *member = module.resolve(member.0, member.1);
                natives::replace(&module.natives, inst);
            }
            Instruction::InvokeStatic(member, _) => {
                *member = module.resolve(member.0, member.1);
                natives::replace(&module.natives, inst);
            }
            _ => {}
        });
    }
    module.functions = functions;
}
