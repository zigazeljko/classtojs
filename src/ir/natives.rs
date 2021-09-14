use serde_derive::Deserialize;
use std::collections::HashMap;
use std::mem;
use ustr::Ustr;

use crate::ir::function::Instruction;
use crate::ir::module::Member;

#[derive(Deserialize, Clone, Copy)]
pub enum Native {
    None,
    IsEqual,
    ToString,
    Var(Ustr),
    Func(Ustr),
    AsyncFunc(Ustr),
    Prop(Ustr),
    Method(Ustr),
    AsyncMethod(Ustr),
}

pub type NativeMap = HashMap<Member, Native>;

pub fn defaults(map: &mut NativeMap) {
    for key in METHOD_TO_OP_NONE {
        map.insert(Member::parse(key).unwrap(), Native::None);
    }
    for key in METHOD_TO_OP_EQUAL {
        map.insert(Member::parse(key).unwrap(), Native::IsEqual);
    }
    for key in METHOD_TO_OP_STRING {
        map.insert(Member::parse(key).unwrap(), Native::ToString);
    }
    for (key, name) in METHOD_TO_VARIABLE {
        map.insert(Member::parse(key).unwrap(), Native::Var((*name).into()));
    }
    for (key, name) in METHOD_TO_FUNCTION {
        map.insert(Member::parse(key).unwrap(), Native::Func((*name).into()));
    }
    for (key, name) in METHOD_TO_ASYNC_FUNCTION {
        map.insert(Member::parse(key).unwrap(), Native::AsyncFunc((*name).into()));
    }
    for (key, name) in METHOD_TO_PROPERTY {
        map.insert(Member::parse(key).unwrap(), Native::Prop((*name).into()));
    }
    for (key, name) in METHOD_TO_METHOD {
        map.insert(Member::parse(key).unwrap(), Native::Method((*name).into()));
    }
}

pub fn replace(map: &NativeMap, inst: &mut Instruction) {
    match inst {
        Instruction::InvokeDirect(member, args) | Instruction::InvokeStatic(member, args) => match map.get(member) {
            Some(Native::None) => {
                *inst = Instruction::GetNativeVar("".into());
            }
            Some(Native::IsEqual) => {
                if let [target, value] = args[..] {
                    *inst = Instruction::ACmp(true, [target, value]);
                }
            }
            Some(Native::ToString) => {
                if let [target] = args[..] {
                    *inst = Instruction::NativeToString([target]);
                }
            }
            Some(Native::Var(name)) => {
                *inst = Instruction::GetNativeVar(*name);
            }
            Some(Native::Func(name)) => {
                *inst = Instruction::InvokeNativeFunc(*name, false, mem::take(args));
            }
            Some(Native::AsyncFunc(name)) => {
                *inst = Instruction::InvokeNativeFunc(*name, true, mem::take(args));
            }
            Some(Native::Prop(name)) => {
                if let [target] = args[..] {
                    *inst = Instruction::GetNativeProp(*name, [target]);
                }
            }
            Some(Native::Method(name)) => {
                *inst = Instruction::InvokeNativeProp(*name, false, mem::take(args));
            }
            Some(Native::AsyncMethod(name)) => {
                *inst = Instruction::InvokeNativeProp(*name, true, mem::take(args));
            }
            None => {}
        },
        _ => {}
    };
}

const METHOD_TO_OP_NONE: &[&str] = &[
    "java.lang.Object.<init>:()V",
    "java.lang.Throwable.fillInStackTrace:()Ljava/lang/Throwable;",
];

const METHOD_TO_OP_EQUAL: &[&str] = &[
    "java.lang.Object.equals:(Ljava/lang/Object;)Z",
    "java.lang.String.equals:(Ljava/lang/Object;)Z",
];

const METHOD_TO_OP_STRING: &[&str] = &[
    "java.lang.Byte.toString:(B)Ljava/lang/String;",
    "java.lang.Double.toString:(D)Ljava/lang/String;",
    "java.lang.Float.toString:(F)Ljava/lang/String;",
    "java.lang.Integer.toString:(I)Ljava/lang/String;",
    "java.lang.Long.toString:(J)Ljava/lang/String;",
    "java.lang.Short.toString:(S)Ljava/lang/String;",
    "java.lang.String.intern:()Ljava/lang/String;",
    "java.lang.String.toString:()Ljava/lang/String;",
];

const METHOD_TO_VARIABLE: &[(&str, &str)] = &[("java.lang.Class.desiredAssertionStatus:()Z", "0")];

const METHOD_TO_FUNCTION: &[(&str, &str)] = &[
    ("java.lang.Double.isNaN:(D)Z", "isNaN"),
    ("java.lang.Float.isNaN:(F)Z", "isNaN"),
    ("java.lang.Math.abs:(D)D", "Math.abs"),
    ("java.lang.Math.abs:(F)F", "Math.abs"),
    ("java.lang.Math.acos:(D)D", "Math.acos"),
    ("java.lang.Math.asin:(D)D", "Math.asin"),
    ("java.lang.Math.atan:(D)D", "Math.atan"),
    ("java.lang.Math.atan2:(DD)D", "Math.atan2"),
    ("java.lang.Math.cbrt:(D)D", "Math.cbrt"),
    ("java.lang.Math.ceil:(D)D", "Math.ceil"),
    ("java.lang.Math.cos:(D)D", "Math.cos"),
    ("java.lang.Math.cosh:(D)D", "Math.cosh"),
    ("java.lang.Math.exp:(D)D", "Math.exp"),
    ("java.lang.Math.expm1:(D)D", "Math.expm1"),
    ("java.lang.Math.floor:(D)D", "Math.floor"),
    ("java.lang.Math.hypot:(D)D", "Math.hypot"),
    ("java.lang.Math.log:(D)D", "Math.log"),
    ("java.lang.Math.log10:(D)D", "Math.log10"),
    ("java.lang.Math.log1p:(D)D", "Math.log1p"),
    ("java.lang.Math.max:(DD)D", "Math.max"),
    ("java.lang.Math.max:(FF)F", "Math.max"),
    ("java.lang.Math.max:(II)I", "Math.max"),
    ("java.lang.Math.min:(DD)D", "Math.min"),
    ("java.lang.Math.min:(FF)F", "Math.min"),
    ("java.lang.Math.min:(II)I", "Math.min"),
    ("java.lang.Math.pow:(DD)D", "Math.pow"),
    ("java.lang.Math.random:()D", "Math.random"),
    ("java.lang.Math.rint:(D)D", "Math.round"),
    ("java.lang.Math.signum:(D)D", "Math.sign"),
    ("java.lang.Math.signum:(F)F", "Math.sign"),
    ("java.lang.Math.sin:(D)D", "Math.sin"),
    ("java.lang.Math.sinh:(D)D", "Math.sinh"),
    ("java.lang.Math.sqrt:(D)D", "Math.sqrt"),
    ("java.lang.Math.tan:(D)D", "Math.tan"),
    ("java.lang.Math.tanh:(D)D", "Math.tanh"),
    ("java.lang.Object.hashCode:()I", "$javahashcode"),
    ("java.lang.StrictMath.abs:(D)D", "Math.abs"),
    ("java.lang.StrictMath.abs:(F)F", "Math.abs"),
    ("java.lang.StrictMath.acos:(D)D", "Math.acos"),
    ("java.lang.StrictMath.asin:(D)D", "Math.asin"),
    ("java.lang.StrictMath.atan:(D)D", "Math.atan"),
    ("java.lang.StrictMath.atan2:(DD)D", "Math.atan2"),
    ("java.lang.StrictMath.cbrt:(D)D", "Math.cbrt"),
    ("java.lang.StrictMath.ceil:(D)D", "Math.ceil"),
    ("java.lang.StrictMath.cos:(D)D", "Math.cos"),
    ("java.lang.StrictMath.cosh:(D)D", "Math.cosh"),
    ("java.lang.StrictMath.exp:(D)D", "Math.exp"),
    ("java.lang.StrictMath.expm1:(D)D", "Math.expm1"),
    ("java.lang.StrictMath.floor:(D)D", "Math.floor"),
    ("java.lang.StrictMath.hypot:(D)D", "Math.hypot"),
    ("java.lang.StrictMath.log:(D)D", "Math.log"),
    ("java.lang.StrictMath.log10:(D)D", "Math.log10"),
    ("java.lang.StrictMath.log1p:(D)D", "Math.log1p"),
    ("java.lang.StrictMath.max:(DD)D", "Math.max"),
    ("java.lang.StrictMath.max:(FF)F", "Math.max"),
    ("java.lang.StrictMath.max:(II)I", "Math.max"),
    ("java.lang.StrictMath.min:(DD)D", "Math.min"),
    ("java.lang.StrictMath.min:(FF)F", "Math.min"),
    ("java.lang.StrictMath.min:(II)I", "Math.min"),
    ("java.lang.StrictMath.pow:(DD)D", "Math.pow"),
    ("java.lang.StrictMath.random:()D", "Math.random"),
    ("java.lang.StrictMath.rint:(D)D", "Math.round"),
    ("java.lang.StrictMath.signum:(D)D", "Math.sign"),
    ("java.lang.StrictMath.signum:(F)F", "Math.sign"),
    ("java.lang.StrictMath.sin:(D)D", "Math.sin"),
    ("java.lang.StrictMath.sinh:(D)D", "Math.sinh"),
    ("java.lang.StrictMath.sqrt:(D)D", "Math.sqrt"),
    ("java.lang.StrictMath.tan:(D)D", "Math.tan"),
    ("java.lang.StrictMath.tanh:(D)D", "Math.tanh"),
    ("java.lang.StringBuilder.<init>:()V", "$javastrbinit"),
    ("java.lang.StringBuilder.<init>:(I)V", "$javastrbinit"),
    ("java.lang.StringBuilder.<init>:(Ljava/lang/String;)V", "$javastrbset"),
    ("java.lang.StringBuilder.append:(B)Ljava/lang/StringBuilder;", "$javastrbpush"),
    ("java.lang.StringBuilder.append:(C)Ljava/lang/StringBuilder;", "$javastrbpushc"),
    ("java.lang.StringBuilder.append:(D)Ljava/lang/StringBuilder;", "$javastrbpush"),
    ("java.lang.StringBuilder.append:(F)Ljava/lang/StringBuilder;", "$javastrbpush"),
    ("java.lang.StringBuilder.append:(I)Ljava/lang/StringBuilder;", "$javastrbpush"),
    ("java.lang.StringBuilder.append:(J)Ljava/lang/StringBuilder;", "$javastrbpush"),
    (
        "java.lang.StringBuilder.append:(Ljava/lang/Object;)Ljava/lang/StringBuilder;",
        "$javastrbpusho",
    ),
    (
        "java.lang.StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;",
        "$javastrbpush",
    ),
    (
        "java.lang.StringBuilder.append:(Ljava/lang/StringBuilder;)Ljava/lang/StringBuilder;",
        "$javastrbpush",
    ),
    ("java.lang.System.arraycopy:(Ljava/lang/Object;ILjava/lang/Object;II)V", "$javaarraycopy"),
    ("java.lang.System.currentTimeMillis:()J", "$javamillitime"),
    ("java.lang.System.nanoTime:()J", "$javananotime"),
];

const METHOD_TO_ASYNC_FUNCTION: &[(&str, &str)] = &[
    ("java.lang.Thread.sleep:(J)V", "$javathreadsleep"),
    ("java.lang.Thread.sleep:(JI)V", "$javathreadsleep"),
    ("java.lang.Thread.yield:(J)V", "$javathreadyield"),
];

const METHOD_TO_PROPERTY: &[(&str, &str)] = &[
    ("java.lang.Class.getName:()Ljava/lang/String;", "name"),
    ("java.lang.Class.toString:()Ljava/lang/String;", "name"),
    ("java.lang.Object.getClass:()Ljava/lang/Class;", "constructor"),
    ("java.lang.String.length:()I", "length"),
    ("java.lang.StringBuilder.toString:()Ljava/lang/String;", "s"),
];

const METHOD_TO_METHOD: &[(&str, &str)] = &[
    ("[Ljava.lang.Object;.clone:()Ljava/lang/Object;", "slice"),
    ("java.lang.String.charAt:(I)C", "charCodeAt"),
    ("java.lang.String.codePointAt:(I)I", "codePointAt"),
    ("java.lang.String.concat:(Ljava/lang/String;)Ljava/lang/String;", "concat"),
    ("java.lang.String.substring:(I)Ljava/lang/String;", "substring"),
    ("java.lang.String.substring:(II)Ljava/lang/String;", "substring"),
];
