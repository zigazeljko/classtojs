use serde_derive::Deserialize;
use std::error::Error;
use std::io;
use std::path::PathBuf;

use crate::codegen;
use crate::ir::class::ClassMap;
use crate::ir::module::{ExportMap, Module};
use crate::ir::natives;
use crate::ir::natives::NativeMap;
use crate::parser;
use crate::pass::devirtualize;
use crate::pass::reachability;

#[derive(Default, Deserialize)]
#[serde(default)]
pub struct Config {
    pub inputs: Vec<PathBuf>,
    pub natives: NativeMap,
    pub export: ExportMap,
    pub minify: bool,
    pub optimize: bool,
}

type Bytes = Box<[u8]>;

impl Config {
    pub fn new(path: PathBuf) -> Config {
        Config {
            inputs: vec![path],
            natives: NativeMap::default(),
            export: ExportMap::default(),
            minify: false,
            optimize: false,
        }
    }
}

fn to_io_error<E: Into<Box<dyn Error + Send + Sync>>>(error: E) -> io::Error {
    io::Error::new(io::ErrorKind::Other, error)
}

pub fn transform_files(data: Vec<Bytes>, mut config: Config) -> io::Result<Vec<u8>> {
    let mut classes = ClassMap::new();
    for buf in data {
        let (name, parsed) = parser::parse(&buf).map_err(to_io_error)?;
        if classes.contains_key(&name) {
            continue;
        }
        let class = parser::transform(parsed).map_err(to_io_error)?;
        classes.insert(name, class);
        if config.export.is_empty() && !name.contains('/') {
            config.export.insert(name, name.to_string());
        }
    }
    natives::defaults(&mut config.natives);
    let mut module = Module::new(classes, config.natives, config.export);
    reachability::apply(&mut module);
    devirtualize::apply(&mut module);
    reachability::apply2(&mut module);
    codegen::mangle::init();
    let output = codegen::generate(&module);
    codegen::stringify(output, config.minify, config.optimize)
}
