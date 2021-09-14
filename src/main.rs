#![feature(slice_group_by)]

use clap::clap_app;
use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::path::Path;
use zip::read::ZipArchive;

mod codegen;
mod ir;
mod parser;
mod pass;
mod transform;

fn read_boxed<T: Read>(mut reader: T) -> io::Result<Box<[u8]>> {
    let mut buf = Vec::new();
    reader.read_to_end(&mut buf)?;
    Ok(buf.into_boxed_slice())
}

fn load_input(out: &mut Vec<Box<[u8]>>, path: &Path) -> io::Result<()> {
    let file = File::open(path)?;
    let ext = path.extension();
    if ext == Some("class".as_ref()) {
        out.push(read_boxed(file)?);
        return Ok(());
    }
    let mut zip = ZipArchive::new(file)?;
    for index in 0..zip.len() {
        let zfile = zip.by_index(index)?;
        if zfile.name().ends_with(".class") {
            out.push(read_boxed(zfile)?);
        }
    }
    Ok(())
}

fn load_config(path: &Path) -> io::Result<transform::Config> {
    let ext = path.extension();
    if ext == Some("class".as_ref()) || ext == Some("jar".as_ref()) {
        return Ok(transform::Config::new(path.to_owned()));
    }
    let file = File::open(path)?;
    let data = read_boxed(file)?;
    Ok(toml::from_slice(&data)?)
}

fn main() -> io::Result<()> {
    let matches = clap_app!(classtojs =>
        (@arg input: +required "Input file or config")
        (@arg output: "Output file name")
        (@arg minify: --minify "Minify output")
        (@arg optimize: --optimize "Optimize output")
    )
    .get_matches();
    let cfgname = matches.value_of_os("input").unwrap();
    let mut config = load_config(Path::new(cfgname))?;
    config.minify |= matches.is_present("minify");
    config.optimize |= matches.is_present("optimize");
    let mut data = Vec::new();
    for name in &config.inputs {
        load_input(&mut data, name)?;
    }
    let code = transform::transform_files(data, config)?;
    match matches.value_of_os("output") {
        Some(path) => File::create(path)?.write_all(&code),
        None => io::stdout().write_all(&code),
    }
}
