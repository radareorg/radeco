//! The Rune Symbolic Emulator Project.
//!
//! Radare2 Symbolic Emulator for all!
//! TODO: Add project notes, descriptions and notes.

// Support additional lints using clippy
#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

extern crate esil;
extern crate libsmt;
extern crate petgraph;
extern crate r2api;
extern crate r2pipe;
extern crate regex;
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;

pub mod context {
    pub mod context;
    pub mod rune_ctx;
}

pub mod utils {
    pub mod state;
    pub mod utils;
}

pub mod memory {
    pub mod memory;
    pub mod qword_mem;
    pub mod seg_mem;
}

pub mod regstore {
    pub mod regfile;
    pub mod regstore;
}

pub mod explorer {
    pub mod bfs;
    pub mod dfs;
    pub mod directed;
    pub mod explorer;
    pub mod interactive;
}

pub mod engine {
    pub mod breakpt;
    pub mod engine;
    pub mod hook;
    pub mod rune;
}

pub mod stream;
