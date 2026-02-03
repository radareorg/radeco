extern crate libsmt;
extern crate r2api;
extern crate r2pipe;
extern crate rune;

use std::collections::HashMap;

use r2pipe::r2::R2;

use r2api::api_trait::R2PApi;

use rune::explorer::directed::DirectedExplorer;
use rune::explorer::PathExplorer;

use rune::engine::rune::Rune;
use rune::engine::Engine;

use rune::utils::{new_rune_ctx, Key};

fn main() {
    // Stream
    let mut stream = R2::new(Some("bins/a.out")).expect("Could not open the file.");
    stream.init().expect("error during R2 stream init");

    let bp = 0x5000;
    let ip = 0x004004fa;
    let break_addr = 0x00400515;
    let branch = 0x00400513;

    let mut syms = HashMap::new();
    syms.insert(Key::Mem(bp - 0x8), 8);

    let mut consts = HashMap::new();
    consts.insert(Key::Reg(String::from("rbp")), (bp as u64, 64 as u64));

    // Context
    let ctx = new_rune_ctx(Some(ip), Some(syms), Some(consts), &mut stream)
        .expect("error creating new Rune context");

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
