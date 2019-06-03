//! `DirectedExplorer`, an implementation of a `PathExplorer` which allows directed symbolic
//! execution

use crate::explorer::explorer::PathExplorer;
use crate::engine::rune::RuneControl;

use crate::context::context::{Context, Evaluate, RegisterRead};
use crate::context::rune_ctx::RuneContext;

use crate::memory::seg_mem::SegMem;
use crate::regstore::regfile::RuneRegFile;

use libsmt::theories::{core};


use std::collections::HashMap;

// I know it is code repitition, but whatevs
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BranchType {
    True,
    False,
    Invalid,
}

// Allows us to maybe provide a function for the user
// to choose which branch to take based on the character.
impl From<char> for BranchType {
    fn from(c: char) -> BranchType {
        match c {
            'T' => BranchType::True,
            'F' => BranchType::False,
            _ => BranchType::Invalid,
        }
    }
}

// For now, let us assume that we have the directed path exploration
// information taken from the user.
// Consider the map of the form Address: BranchType.
// This will allow us to choose the ideal branch to be taken at each 
// instance of branching into a different path
#[derive(Debug, Clone, Default)]
pub struct DirectedExplorer {
    pub d_map: HashMap<u64, BranchType>,
    pub break_addr: u64,
    // Ideally we should give control back to the user based on
    // his choice to stop/start execution somewhere.
}

impl PathExplorer for DirectedExplorer
{
    type C = RuneControl;
    type Ctx = RuneContext<SegMem, RuneRegFile>;

   fn new() -> Self {
       DirectedExplorer {
           d_map: HashMap::new(),
           break_addr: 0x0000,
       }
   }
   
    fn next(&mut self, ctx: &mut Self::Ctx) -> RuneControl {
        // Automated continuous exploration my bois
        if ctx.ip() == self.break_addr {
            println!("[+] The Engine is now halted.");
            return RuneControl::Halt
        }
        RuneControl::Continue
    }

    fn next_job(&mut self, _ctx: &mut Self::Ctx) -> Option<RuneControl> {
        None
    }

    fn register_branch(&mut self,
                       ctx: &mut Self::Ctx,
                       condition: <Self::Ctx as RegisterRead>::VarRef)
        -> RuneControl {
            // println!("{:#x}", ctx.ip());
            if self.d_map.contains_key(&ctx.ip()) {
                let direction = self.d_map.get(&ctx.ip()).unwrap();
                match *direction {
                    BranchType::True => {
                        let one = ctx.define_const(1, 64);
                        ctx.eval(core::OpCodes::Cmp, &[condition, one]);
                        RuneControl::ExploreTrue
                    }
                    BranchType::False => {
                        let zero = ctx.define_const(0, 64);
                        ctx.eval(core::OpCodes::Cmp, &[condition, zero]);
                        RuneControl::ExploreFalse
                    }
                    _ => panic!("Invalid branch type found!"),
                }
            } else {
                panic!("Do not know which branch to take. Set a default!");
            }
    }
}

impl DirectedExplorer {
    pub fn set_decisions(&mut self, decision_list: Vec<(u64, char)>) {
        let mut d_map: HashMap<u64, BranchType> = HashMap::new();
        
        for tup in decision_list {
            d_map.entry(tup.0).or_insert(BranchType::from(tup.1));
        }

        self.d_map = d_map;
    }
}
