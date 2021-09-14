use std::io;
use swc_common::source_map as sm;
use swc_common::source_map::DUMMY_SP;
use swc_common::sync::Lrc;
use swc_common::Mark;
use swc_ecma_ast as es;
use swc_ecma_codegen as cg;
use swc_ecma_minifier as min;
use swc_ecma_utils::quote_str;
use swc_ecma_visit::Fold;
use ustr::Ustr;

use crate::ir::module;
use crate::ir::module::{InitValue, Member, Module};

mod function;
pub mod mangle;
mod util;

fn initvalue(value: &InitValue) -> es::Expr {
    match value {
        InitValue::Null => es::Lit::Null(util::NULL).into(),
        InitValue::Number(val) => util::number(*val),
        InitValue::Long(val) => util::long(*val).into(),
        InitValue::String(val) => quote_str!(val.to_string()).into(),
    }
}

fn defaultinit(desc: Ustr) -> es::Expr {
    match desc.as_str() {
        "B" | "C" | "D" | "F" | "I" | "S" | "Z" => 0.0.into(),
        "J" => util::long(0).into(),
        _ => es::Lit::Null(util::NULL).into(),
    }
}

pub fn generate(module: &Module) -> es::Module {
    let mut body = Vec::new();
    for (name, mfunc) in &module.functions {
        let func = function::generate(mfunc);
        let ident = mangle::global(name.0, name.1);
        let decl = util::function_decl(ident, func);
        body.push(decl.into());
    }
    for (name, init) in &module.variables {
        let expr = initvalue(init);
        let ident = mangle::global(name.0, name.1);
        let decl = util::var(vec![util::vari(ident, Some(expr))]);
        body.push(decl.into());
    }
    for (class, data) in &module.prototypes {
        let array = class.starts_with('[');
        if mangle::CLASS_BUILTINS.contains(&class.as_str()) {
            continue;
        }
        let mut decls = Vec::new();
        for name in &data.fields {
            let member = util::field(mangle::member(*name), defaultinit(name.1));
            decls.push(member.into());
        }
        for name in &data.methods {
            let resolved = module.resolve(*class, *name);
            if resolved.0 == "java/lang/Object" {
                continue;
            }
            if module.functions.contains_key(&resolved) {
                let func = function::generate_proxy(resolved.0, resolved.1);
                let member = util::method(mangle::member(*name), func);
                decls.push(member.into());
            }
            if let Some(native) = module.natives.get(&resolved) {
                let func = function::generate_native_proxy(*native, resolved.1);
                let member = util::method(mangle::member(*name), func);
                decls.push(member.into());
            }
        }
        for (name, isstatic) in &data.exports {
            let resolved = module.resolve(*class, *name);
            if !isstatic && module.functions.contains_key(&resolved) {
                let func = function::generate_proxy(resolved.0, resolved.1);
                let member = util::method(mangle::exported(*name), func);
                decls.push(member.into());
            }
        }
        let supername = match array {
            true => Some(util::ident("Array".into()).into()),
            false => None,
        };
        let decl = util::class_decl(mangle::class(*class), util::class(decls, supername));
        body.push(decl.into());
    }
    for class in module.prototypes.keys() {
        if let Some(eclass) = module::element_array_type(class) {
            let member = util::imember(mangle::class(*class).into(), util::ident("elementType".into()));
            let assign = util::eassign(es::AssignOp::Assign, member, mangle::class(eclass).into());
            body.push(util::wrap(assign));
        }
        for name in &module.superclass[class] {
            if !module.instances.contains(name) {
                continue;
            }
            let member = util::imember(mangle::class(*class).into(), mangle::instance(*name, 1));
            let assign = util::eassign(es::AssignOp::Assign, member, (1.0).into());
            body.push(util::wrap(assign));
        }
    }
    let clinit = ("<clinit>".into(), "()V".into());
    for class in &module.inits {
        if module.functions.contains_key(&Member(*class, clinit)) {
            let ident = mangle::global(*class, clinit);
            body.push(util::wrap(util::call(ident.into(), vec![])));
        }
    }
    for (class, data) in &module.prototypes {
        for (name, isstatic) in &data.exports {
            let resolved = module.resolve(*class, *name);
            if *isstatic && module.functions.contains_key(&resolved) {
                let member = util::imember(mangle::class(*class).into(), mangle::exported(*name));
                let function = mangle::global(resolved.0, resolved.1).into();
                let assign = util::eassign(es::AssignOp::Assign, member, function);
                body.push(util::wrap(assign));
            }
        }
    }
    let exports = util::ident("exports".into());
    for (class, name) in &module.exports {
        if name.is_empty() {
            continue;
        }
        let member = util::imember(exports.clone().into(), util::ident(name.into()));
        let assign = util::eassign(es::AssignOp::Assign, member, mangle::class(*class).into());
        body.push(util::wrap(assign));
    }
    let wrapper = util::wrap(util::call(
        util::fnexpr(util::function(vec![util::param(exports)], body, false)),
        vec![util::THIS.into()],
    ));
    es::Module {
        span: DUMMY_SP,
        body: vec![wrapper.into()],
        shebang: None,
    }
}

pub fn stringify(module: es::Module, minify: bool, optimize: bool) -> io::Result<Vec<u8>> {
    let globals = swc_common::Globals::new();
    let sourcemap = Lrc::new(sm::SourceMap::new(sm::FilePathMapping::empty()));
    let result = swc_common::GLOBALS.set(&globals, || {
        if !optimize {
            return module;
        }
        let compress = min::option::CompressOptions {
            inline: 0,
            ..Default::default()
        };
        let options = min::option::MinifyOptions {
            rename: false,
            compress: Some(compress),
            mangle: None,
            wrap: false,
            enclose: false,
        };
        let extra = min::option::ExtraOptions {
            top_level_mark: Mark::fresh(Mark::root()),
        };
        min::optimize(module, sourcemap.clone(), None, None, &options, &extra)
    });
    let mut fixer = swc_ecma_transforms::fixer(None);
    let fixed = fixer.fold_module(result);
    let mut buf = Vec::new();
    {
        let writer = cg::text_writer::JsWriter::new(sourcemap.clone(), "\n", &mut buf, None);
        let mut emitter = cg::Emitter {
            cfg: cg::Config { minify },
            cm: sourcemap,
            comments: None,
            wr: Box::new(writer),
        };
        emitter.emit_module(&fixed)?;
    }
    Ok(buf)
}
