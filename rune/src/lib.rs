//! The Rune Symbolic Emulator Project.
//!
//! Radare2 Symbolic Emulator for all!
//! TODO: Add project notes, descriptions and notes.

// Support additional lints using clippy
#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

extern crate petgraph;
extern crate esil;
extern crate r2pipe;
extern crate r2api;
extern crate rustc_serialize;
extern crate regex;
extern crate libsmt;
#[macro_use] extern crate serde_derive;
extern crate serde;
extern crate serde_json;

pub mod context {
    pub mod context;
    pub mod rune_ctx;
}

pub mod utils {
    pub mod utils;
    pub mod state;
}

pub mod memory {
    pub mod memory;
    pub mod qword_mem;
    pub mod seg_mem;
}

pub mod regstore {
    pub mod regstore;
    pub mod regfile;
}

pub mod explorer {
    pub mod explorer;
    pub mod dfs;
    pub mod bfs;
    pub mod interactive;
    pub mod directed;
}

pub mod engine {
    pub mod engine;
    pub mod rune;
    pub mod hook;
    pub mod breakpt;
}

pub mod stream;
