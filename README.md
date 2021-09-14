# classtojs

An ahead-of-time Java-to-JavaScript compiler written in Rust.

## Installation

classtojs can be built from source using [cargo](https://doc.rust-lang.org/cargo/).

```
git clone https://github.com/zigazeljko/classtojs.git
cd classtojs
cargo build
```

## Usage

Compiling a single class file to JavaScript:

```
classtojs Example.class out.js
```

Compiling multiple files using a config file:

```
classtojs example.toml out.js
```

## Demos

Demos are available in the [classtojs-demos](https://github.com/zigazeljko/classtojs-demos) repository.

Online demos: [JBox2D](https://zigazeljko.github.io/classtojs-demos/jbox2d/) and [Minecraft4k](https://zigazeljko.github.io/classtojs-demos/minecraft4k/).
