#![doc(html_root_url = "https://radare.github.io/radeco-lib/")]
#![doc(html_logo_url = "http://rada.re/r/img/r2logo3.png")]
extern crate regex;
extern crate petgraph;
extern crate rustc_serialize;
extern crate num;

#[macro_use]
extern crate r2pipe;

pub mod analysis;
pub mod backend;
pub mod frontend;
pub mod middle;
pub mod pipeline;
pub mod web;
