# Radeco

[![Appveyor Status](https://ci.appveyor.com/api/projects/status/t8dujay25g31sxia?svg=true)](https://ci.appveyor.com/project/radare/radeco-lib-8ycg0)
[![Build Status](https://travis-ci.org/radareorg/radeco.svg?branch=master)](https://travis-ci.org/radareorg/radeco)
[![Coverage Status](https://coveralls.io/repos/github/radare/radeco-lib/badge.svg?branch=master)](https://coveralls.io/github/radare/radeco-lib?branch=master)

A radare2 based binary analysis framework consisting from the Radeco client, in `./radeco/` directory, `./radeco-lib/` - library where whole high-level logic is located, `./arch-rs/` to abstract the architectures intricacies, `./esil-rs/` to parse the radare2 ESIL, and `./rune/` to perform symbolic execution on top of ESIL. Radeco uses its own intermediate representation,
which also has a text representation - RadecoIL.

### Is this ready yet?

Nope. There is still a ton of work to do before this can be considered ready.
That said, parts of the library are already stable enough to write your own
analysis passes and use in your projects.

## Usage

Build like a regular rust project, using cargo:

`cargo build`

To include in your rust project, add to Cargo.toml:

```
[dependencies.radeco-lib]
git = "https://github.com/radare/radeco"
```

See examples for usage.

### Trace Log

To debug, you may want to enable trace output from various parts of radeco.
Build with `trace_log` feature to enable this:

`cargo build --features 'trace_log'`


### Profiling

Requires [gperftools ](https://github.com/gperftools/gperftools). Check the
[cpuprofiler](https://github.com/AtheMathmo/cpuprofiler) repository for more details.

To enable profiling, build with `profile` feature:

`cargo build --features 'profiler'`

Wrap the code you want to profile with:

```rust
use cpuprofiler::PROFILER;

PROFILER.lock().unwrap().start("./my-prof.profile").unwrap();
// Code you want to sample goes here!
PROFILER.lock().unwrap().stop().unwrap();
```

## Radeco-lib project layout

```
src/
├── analysis/               Analyzers on SSA form Radeco-IR
├── backend/                Analyzers on C-pseudo code
│   ├── ctrl_flow_struct/   Implementation of `No More Gotos`
│   └── lang_c/             Coverter of C-pseudo code from RadecoFunction
├── frontend/               Loaders of RadecoFunction, RadecoProject
├── middle/                 Constructer, writer, parser of Radeco-IR
│   ├── regfile/            Profile of registers
│   └── ssa/                SSA form of Radeco-IR
└── utils/                  Logger, etc
```

## License
Licensed under The BSD 3-Clause License. Please check [COPYING](https://github.com/radare/radeco-lib/blob/master/COPYING) file for
complete license.
