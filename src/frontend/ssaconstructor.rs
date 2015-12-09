//! This module uses the SSA Methods defined to contstruct the SSA form
//! straight from raw esil

// TODO: Remove these lints
#![allow(unused_imports, unused_variables, dead_code)]

use std::collections::HashMap;
use petgraph::graph::NodeIndex;
use std::fmt::Debug;
use std::convert::From;

use r2pipe::structs::LRegInfo;

use esil::parser::{Parser};
use esil::lexer::Tokenizer;

use middle::cfg::NodeData as CFGNodeData;
use middle::cfg::EdgeType as CFGEdgeType;
use middle::cfg::{BasicBlock, CFG};
use middle::dce;
use middle::ir::{MInst, MOpcode, MVal, MValType};
use middle::phiplacement::PhiPlacer;
use middle::regfile::SubRegisterFile;
use middle::ssa::{BBInfo, SSA, SSAExtra, SSAMod};
use middle::ssa::verifier::{VerifiedAdd, Verify};

pub type VarId = usize;

const ESIL_CUR: usize = 0;
const ESIL_OLD: usize = 1;
const LASTSZ: usize = 2;

pub struct SSAConstruct<'a, T>
    where T: 'a + Clone + Debug + SSAMod<BBInfo = BBInfo>
{
    phiplacer: PhiPlacer<'a, T>,
    regfile: SubRegisterFile,
}

impl<'a, T> SSAConstruct<'a, T>
where T: 'a + Clone + Debug + Verify + SSAExtra +
         SSAMod<BBInfo=BBInfo, ValueRef=NodeIndex, ActionRef=NodeIndex>
{
    pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruct<'a, T> {
        let mut sc = SSAConstruct {
            phiplacer: PhiPlacer::new(ssa),
            regfile: SubRegisterFile::new(reg_info),
        };

        // Add esil_cur, esil_old, and esil_lastsz to the variable list.
        sc.phiplacer.add_variables(vec![
                                   From::from(64 as u16),
                                   From::from(64 as u16),
                                   From::from(64 as u16),
        ]);

        // Add all the registers to the variable list.
        sc.phiplacer.add_variables(sc.regfile.whole_registers.clone());
        sc
    }

    pub fn run(&mut self) {
        unimplemented!()
    }
}
