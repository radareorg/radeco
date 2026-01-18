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

pub mod context;
pub mod engine;
pub mod explorer;
pub mod memory;
pub mod regstore;
pub mod stream;
pub mod utils;
