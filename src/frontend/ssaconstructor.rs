//! This module uses the SSA Methods defined to contstruct the SSA form
//! straight from raw esil

// TODO: Remove these lints
#![allow(unused_imports, unused_variables, dead_code)]

use std::collections::HashMap;
use petgraph::graph::NodeIndex;
use std::fmt::Debug;
use std::convert::From;

use r2pipe::structs::LRegInfo;

use esil::parser::{Parse, Parser};
use esil::lexer::{Token, Tokenizer};

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
    blocks: Vec<T::ActionRef>,
}

impl<'a, T> SSAConstruct<'a, T>
    where T: 'a + Clone
    + Debug
    + Verify
    + SSAExtra
    + SSAMod<BBInfo=BBInfo, ValueRef=NodeIndex, ActionRef=NodeIndex>
{
    pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruct<'a, T> {
        let mut sc = SSAConstruct {
            phiplacer: PhiPlacer::new(ssa),
            regfile: SubRegisterFile::new(reg_info),
            blocks: Vec::new(),
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

    // If the operand is a Token::Identifier, it has to be a register.
    // This is because we never push in a temporary that we create as a
    // Token::Identifier and all ESIL identifiers must be a valid register.
    // It will remain as a Token::Identifier only the first time the parser
    // encounters
    // it, we always push in a Token::Register or a Token::Intermediate.
    fn process_in(&mut self, var: Option<Token>, address: u64) -> Option<T::ValueRef> {
        unimplemented!();
    }

    fn process_out(&mut self, result: T::ValueRef, address: u64) -> Option<Token> {
        unimplemented!();
    }

    fn process_op(&mut self,
                  token: &Token,
                  address: u64,
                  operands: &[Option<T::ValueRef>; 2])
                  -> T::ValueRef {
        unimplemented!();
    }

    // Returns a reference to the arry index that was created for this block.
    // start_addr is the start address of this basic block.
    fn add_block(&mut self, start_addr: u64) -> usize {
        let block = self.phiplacer.add_block(BBInfo { addr: start_addr });
    }

    fn init_blocks(&mut self) -> T::ActionRef {
        // Create a start block with all registers as variables defined in this block.
        // Seal this block as the start block cannot have any more successors.
        // Create another block and return as "current_block" that we are processing.

        let start_block = self.phiplacer.add_block(BBInfo { addr: 0 });
        self.phiplacer.ssa.mark_start_node(&start_block);

        let zero = self.phiplacer.ssa.add_const(&start_block, 0);
        // TODO: This might not be necessary anymore.
        self.phiplacer.write_variable(start_block, ESIL_CUR, zero);
        self.phiplacer.write_variable(start_block, ESIL_OLD, zero);
        self.phiplacer.write_variable(start_block, ESIL_LASTSZ, zero);

        for (i, name) in self.regfile.whole_names.iter().enumerate() {
            let reg = self.regfile.whole_registers[i];
            // Name the newly created nodes with register names.
            let argnode = self.phiplacer.ssa.add_comment(block, reg, name);
            // 0, 1 and 2 are esilcur, esilold and lastsz respectively
            self.phiplacer.write_variable(block, i + 3, argnode);
        }

        self.phiplacer.sync_register_state(start_block);
    }

    // For now, some other component provides SSAConstruct with the instructions
    // that it is
    // supposed to convert into SSA. SSAConstruct does not care from where this
    // ESIL is received,
    // it merely takes this vector of ESIL strings and transforms it into its SSA
    // form.
    pub fn run<S: AsRef<str>>(&mut self, esil: Vec<S>) {
        let mut p = Parser::init(None);
        let mut current_block = self.init_blocks();
        for i in &esil {
            while let Some(ref token) = p.parse::<_, Tokenizer>(i) {
                let (_lhs, _rhs) = p.fetch_operands(token);
                let lhs = self.process_in(_lhs, 0);
                let rhs = self.process_in(_rhs, 0);
                // Determine what to do with the operands and get the result.
                let result = self.process_op(token, 0, &[lhs, rhs]);
                if let Some(_result) = self.process_out(result, 0) {
                    p.push(_result);
                }
            }
        }
    }

} // end impl SSAConstruct
