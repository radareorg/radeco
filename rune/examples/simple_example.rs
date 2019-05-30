extern crate rune;
extern crate libsmt;
extern crate r2pipe;
extern crate r2api;

use std::collections::HashMap;

use r2pipe::r2::R2;
use r2api::structs::LRegInfo;
use r2api::api_trait::R2Api;

use rune::explorer::directed::DirectedExplorer;
use rune::explorer::explorer::PathExplorer;

use rune::context::rune_ctx::RuneContext;
use rune::context::context::{ContextAPI};

use rune::memory::memory::Memory;
use rune::memory::seg_mem::SegMem;

use rune::regstore::regstore::RegStore;
use rune::regstore::regfile::RuneRegFile;

use rune::engine::engine::Engine;
use rune::engine::rune::Rune;

use rune::utils::utils::{new_rune_ctx, Key};

use libsmt::backends::smtlib2::SMTLib2;

fn main() {
    // Stream
    let mut stream = R2::new(Some("bins/a.out")).expect("Could not open the file.");
    stream.init();

    let bp = 0x5000;
    let ip = 0x004004fa;
    let break_addr = 0x00400515;
    let branch = 0x00400513;

    let mut syms = HashMap::new();
    syms.insert(Key::Mem(bp-0x8), 8);

    let mut consts = HashMap::new();
    consts.insert(Key::Reg(String::from("rbp")), (bp as u64, 64 as u64));

    // Context
    let mut ctx = new_rune_ctx(Some(ip), Some(syms), Some(consts), &mut stream);

    // Explorer
    let mut explorer = DirectedExplorer::new();
    let mut decision_list: Vec<(u64, char)> = Vec::new();
    decision_list.push((branch, 'F'));
    explorer.set_decisions(decision_list);
    explorer.break_addr = break_addr;

    // Engine
    let mut rune = Rune::new(ctx, explorer, stream);
    rune.run().expect("not yet implemented");
}
