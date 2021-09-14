use cached::proc_macro::cached;
use cached::Cached;
use std::sync::atomic::{AtomicUsize, Ordering};
use swc_common::source_map::DUMMY_SP;
use swc_ecma_ast as es;
use swc_ecma_ast::IdentExt;
use ustr::Ustr;

use crate::ir::module::NameAndType;

const CHARS: [u8; 52] = *b"aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ";

static GLOBALS: AtomicUsize = AtomicUsize::new(53);
static MEMBERS: AtomicUsize = AtomicUsize::new(52);

fn base52(mut n: usize) -> String {
    let mut ret = String::new();
    n += 1;
    while n > 0 {
        n -= 1;
        ret.push(CHARS[n % 52] as char);
        n /= 52;
    }
    ret
}

pub fn ident(n: &mut usize) -> es::Ident {
    let mut ret = base52(*n);
    if ret.is_reserved() {
        *n += 2;
        ret = base52(*n);
    }
    *n += 2;
    es::Ident::new(ret.into(), DUMMY_SP)
}

pub fn exported(name: NameAndType) -> es::Ident {
    if name.0 == "<init>" {
        es::Ident::new("$init".into(), DUMMY_SP)
    } else {
        es::Ident::new(name.0.as_str().into(), DUMMY_SP)
    }
}

#[cached]
pub fn class(_name: Ustr) -> es::Ident {
    let ret = base52(GLOBALS.fetch_add(2, Ordering::Relaxed));
    es::Ident::new(ret.into(), DUMMY_SP)
}

#[cached]
pub fn instance(name: Ustr, prefix: usize) -> es::Ident {
    let mut ret = String::from("$").repeat(prefix);
    for ch in name.bytes() {
        match ch as char {
            '0'..='9' | 'A'..='Z' | 'a'..='z' | '_' | '$' => ret.push(ch as char),
            '/' | '[' => ret.push('$'),
            _ => {}
        }
    }
    es::Ident::new(ret.into(), DUMMY_SP)
}

#[cached]
pub fn member(_name: NameAndType) -> es::Ident {
    let mut ret = base52(MEMBERS.fetch_add(2, Ordering::Relaxed));
    if ret.is_reserved() {
        ret = base52(MEMBERS.fetch_add(2, Ordering::Relaxed));
    }
    es::Ident::new(ret.into(), DUMMY_SP)
}

#[cached]
pub fn global(_class: Ustr, _name: NameAndType) -> es::Ident {
    let ret = base52(GLOBALS.fetch_add(2, Ordering::Relaxed));
    es::Ident::new(ret.into(), DUMMY_SP)
}

pub fn class_e(name: Ustr) -> es::Expr {
    class(name).into()
}

pub const CLASS_BUILTINS: &[&str] = &[
    "[Ljava/lang/Object;",
    "java/lang/Class",
    "java/lang/String",
    "[Z",
    "[C",
    "[F",
    "[D",
    "[B",
    "[S",
    "[I",
    "[J",
];

const CLASS_MAPPING: &[(&str, &str)] = &[
    ("[Ljava/lang/Object;", "Array"),
    ("java/lang/Class", "Function"),
    ("java/lang/String", "String"),
    ("[Z", "Uint8Array"),
    ("[C", "Uint16Array"),
    ("[F", "Float32Array"),
    ("[D", "Float64Array"),
    ("[B", "Int8Array"),
    ("[S", "Int16Array"),
    ("[I", "Int32Array"),
    ("[J", "BigInt64Array"),
];

const MEMBER_MAPPING: &[(&str, &str, &str)] = &[
    ("clone", "()Ljava/lang/Object;", "_clone"),
    ("equals", "(Ljava/lang/Object;)Z", "_equals"),
    ("hashCode", "()I", "_hash"),
    ("toString", "()Ljava/lang/String;", "_tostr"),
];

pub fn init() {
    if let Ok(mut map) = CLASS.lock() {
        for (name, mapped) in CLASS_MAPPING {
            let ident = es::Ident::new(mapped.to_string().into(), DUMMY_SP);
            map.cache_set((*name).into(), ident);
        }
    }
    if let Ok(mut map) = MEMBER.lock() {
        for (name, desc, mapped) in MEMBER_MAPPING {
            let ident = es::Ident::new(mapped.to_string().into(), DUMMY_SP);
            map.cache_set(((*name).into(), (*desc).into()), ident);
        }
    }
}
