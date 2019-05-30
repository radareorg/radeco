//! This module offers structs and traits for valueset analysis as inctroduced
//! in "Analyzing Memory Access in x86 Executables" by Gogul Balakrishnan and
//! Thomas Reps
//! For a more complete work on this topic, see the dissertation(!) (there is
//! also an article with the same title) of Gogul Balakrishnan:
//! "WYSINWYX: WHAT YOU SEE IS NOT WHAT YOU EXECUTE"
//! It offers datastructures specific to memory access
//! VSA (value-set analysis) analyzes access patterns on memory
//! an a-loc is an "abstract location" representing roughly a variable in C
//! This implementation is still work in progress.

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use petgraph::graph::{NodeIndex,EdgeIndex};

use super::{StridedInterval_u,AbstractValue};
use super::mem_structs::{AbstractStore,MemRegion,A_Loc,AbstractAddress};
use super::mem_structs::{MemRegionType};

use frontend::containers::{RModule,RadecoModule,RFunction,RadecoFunction};
use frontend::bindings::{RBindings,RadecoBindings,Binding};
use frontend::source::Source;
use middle::ssa::ssa_traits::NodeData as TNodeData;
use middle::ssa::ssa_traits::{SSA,NodeType,ValueType};
use middle::ssa::ssastorage::{NodeData,SSAStorage};
use middle::ir::{MOpcode,MAddress,MArity};
use middle::ir_writer::{IRWriter};

use r2api::structs::{LRegInfo};

//use esil::parser::{Parse, Parser};
//use esil::lexer::{Token, Tokenizer};

// General Notes:
// for efficient impl of abstract store use "applicative dictionaries"
// (see end of Analyzing Memory Accesses in x86 Executables end of sec 3)
// each instruction (node in our case?) takes an abstract store as input
// and outputs one - this implementation does have to adapt this
// implement those 'transformers'

fn perform_op(op: MOpcode, operands: Vec<i64>) -> i64 {
    debug!("\t\t\tperform op: {:?}, {:?}", op, operands);
    match op {
            MOpcode::OpAdd => operands[0] + operands[1],
            MOpcode::OpSub => operands[0] - operands[1],
            //MOpcode::OpMul => operands[0] * operands[1], //FIXME panics on overflow
            MOpcode::OpMul => {
                let (res, overflow) = operands[0].overflowing_mul(operands[1]);
                // Actually I do not know how to hande this correctly.
                // (Even _whether_ this is to be handled.)
                if overflow {warn!("Multiplication overflowed!")};
                res
            },
            MOpcode::OpDiv => operands[0] / operands[1],
            MOpcode::OpMod => operands[0] % operands[1],
            MOpcode::OpAnd => operands[0] & operands[1],
            MOpcode::OpOr  => operands[0] | operands[1],
            MOpcode::OpXor => operands[0] ^ operands[1],
            MOpcode::OpNot => !operands[0],
            //MOpcode::OpEq => operands[0] == operands[1],
            //MOpcode::OpGt  => operands[0] - operands[1],
            //MOpcode::OpLt  => operands[0] - operands[1],
            MOpcode::OpLsl => operands[0] << operands[1],
            MOpcode::OpLsr => operands[0] >> operands[1],
            //MOpcode::OpNarrow(_) => ("narrow", MArity::Unary),
            //MOpcode::OpWiden(_) => ("widen", MArity::Unary),
            MOpcode::OpConst(c) => c as i64,
            _ => 0,
    }
}

//TODO get information about available registers/architecture/...
//from somewhere (ssa?)
fn is_stack_pointer(comment: &String) -> bool {
    match comment.as_ref() {
        "rsp" | "esp" => {true}
        _ => {false}
    }
}
fn is_base_pointer(comment: &String) -> bool {
    match comment.as_ref() {
        "rbp" | "ebp" => {true}
        _ => {false}
    }
}
fn is_gen_purpose_reg(comment: &String) -> bool {
    match comment.as_ref() {
        "rax" | "eax" |
        "rbx" | "ebx" |
        "rcx" | "ecx" |
        "rdx" | "edx" |
        "rdi" | "edi" |
        "rsi" | "esi" |
        "r11"
        | "af" | "cf" | "of" | "pf" | "sf" | "tf" | "zf" | "ds"
        => {true}
        _ => {false}
    }
}
fn is_instruction_pointer(comment: &String) -> bool {
    match comment.as_ref() {
        "rip" | "eip" => {true}
        _ => {false}
    }
}
fn is_register(comment: &String) -> bool {
    is_stack_pointer(comment)
        | is_base_pointer(comment)
        | is_gen_purpose_reg(comment)
        | is_instruction_pointer(comment)
}


pub struct FnAnalyzer<RFn>
where RFn : RFunction + Clone
{
    //rfn: RFn,
    ssa: RFn::SSA,
    a_store_fn: AbstractStore<<<RFn as RFunction>::SSA as SSA>::ValueRef>,
    mem_reg_local: MemRegion,
    stack_size: Option<u64>,
}

impl<RFn> FnAnalyzer<RFn>
where RFn: RFunction + Clone,
      <RFn as RFunction>::SSA: Clone
{
    pub fn from(rfn: RFn) -> FnAnalyzer<RFn> {
        FnAnalyzer {
            //rfn: rfn,
            ssa: (*rfn.ssa_ref()).clone(),
            a_store_fn: AbstractStore::new(),
            mem_reg_local: MemRegion::new(MemRegionType::Local),
            stack_size: None,
        }
    }

    /// Print the SSA node as one expression
    /// for example: load((rax + 42) - 23)
    fn print_node_as_comp(&self,
                          node: <<RFn as RFunction>::SSA as SSA>::ValueRef)
    -> String {
        let op_type = self.ssa.get_node_data(&node).expect("No node data.").nt;
        //debug!("print_node: {:?}", op_type);
        match op_type {
            NodeType::Op(opcode) => {
                let ops = self.ssa.get_operands(&node);
                match opcode.arity() {
                    MArity::Zero => {
                        match opcode {
                            MOpcode::OpConst(c) => format!("{}", c),
                            _ => format!("{}", opcode.to_string()),
                        }
                    },
                    MArity::Unary => {
                        format!("{}{}",
                                opcode.to_string(),
                                self.print_node_as_comp(ops[0]))
                    },
                    MArity::Binary => {
                        match opcode {
                            MOpcode::OpLoad => {
                                //format!("{}({}, {})",
                                //        opcode.to_string(),
                                //        self.print_node_as_comp(ops[0]),
                                //        self.print_node_as_comp(ops[1]))
                                //},
                                format!("{}({})",
                                         opcode.to_string(),
                                         self.print_node_as_comp(ops[1]))
                            },
                            MOpcode::OpStore => {
                                // FIXME probably has wrong arity
                                //format!("{}({}, {}, {})",
                                //        opcode.to_string(),
                                //        self.print_node_as_comp(ops[0]),
                                //        self.print_node_as_comp(ops[1]),
                                //        self.print_node_as_comp(ops[2]))
                                format!("{}({}, {})",
                                        opcode.to_string(),
                                        self.print_node_as_comp(ops[1]),
                                        self.print_node_as_comp(ops[2]))
                            },
                            _ => format!("({} {} {})",
                                         self.print_node_as_comp(ops[0]),
                                         opcode.to_string(),
                                         self.print_node_as_comp(ops[1])),
                        }
                    },
                    MArity::Ternary => {
                        format!("{}({}, {}, {})",
                                opcode.to_string(),
                                self.print_node_as_comp(ops[0]),
                                self.print_node_as_comp(ops[1]),
                                self.print_node_as_comp(ops[2]))
                    },
                }
            },
            NodeType::Comment(c) => format!("{}", c),
            //NodeType::Phi => format!("(Phi)"),
            NodeType::Phi => {
                let mut ret = format!("Phi(");
                let ops = self.ssa.get_operands(&node);
                for op in ops {
                    ret = ret + &format!("{}, ", self.print_node_as_comp(op));
                }
                ret + &format!(")")
            },
            NodeType::Undefined => format!("(Undefinded optype)"),
        }
    }

    /// When a load operation is given, it tries to load
    /// something from a given aloc.
    /// This function traverses all store operations,
    /// checks whether the given a-loc matches,
    ///     if so, return value stored to a-loc,
    ///     else , return uninitialized value
    /// Takes as arguments the store node, and the a-loc
    fn compute_loaded_value(&self,
                            node: <<RFn as RFunction>::SSA as SSA>::ValueRef,
                            (a_loc_base, a_loc_offs):
                                (A_Loc<<<RFn as RFunction>::SSA as SSA>::ValueRef>,
                                 i64))
    -> StridedInterval_u
    {
        debug!("compute_loaded_value({:?})", node);
        debug!("\tcalc: {}", self.print_node_as_comp(node));
        debug!("\ta-loc base: {}, offs: {}", a_loc_base, a_loc_offs);
        let node_data = self.ssa.get_node_data(&node).expect("No node data.");
        let op_type = node_data.nt;
        let operands = self.ssa.get_operands(&node);
        //debug!("\toperands: {:?}", operands);
        //debug!("\t\t{:?} - {:?}", op_type, node);
        //for op in &operands {
        //    let nd = self.ssa.get_node_data(&op).expect("No node data.");
        //    debug!("\t\t\t{:?}", nd.nt);
        //}
        match op_type {
            NodeType::Comment(ref c) if c.eq("mem") => {
                //debug!("\t\t\t\tNo matching a-loc found - ret uninitialized");
                // We are loading from a memory region of which we don't know the value
                StridedInterval_u::new()
            },
            NodeType::Op(MOpcode::OpStore) => {
                let mem_state   = operands[0];
                let target_node = operands[1];
                let value_node  = operands[2];
                let (a_loc_base_stored, a_loc_offs_stored) =
                    self.compute_a_loc(target_node)
                    .expect("No base a-loc to store operation");
                debug!("\t\t\t\tcomparing to:\n\t\t\t{}, {}",
                       a_loc_base_stored, a_loc_offs_stored);
                if (a_loc_base.clone(), a_loc_offs) ==
                    (a_loc_base_stored.clone(), a_loc_offs_stored) {
                        debug!("\t\t\t\tmatching - found storing location, get stored value");
                        self.compute_abstract_value(value_node)
                    } else {
                        debug!("\t\t\t\tNo matching a-loc - continuing search");
                        self.compute_loaded_value(mem_state, (a_loc_base, a_loc_offs))
                    }
            },
            NodeType::Phi => {
                let loaded_val1 =
                    self.compute_loaded_value(operands[0],
                                              (a_loc_base.clone(), a_loc_offs));
                let loaded_val2 =
                    self.compute_loaded_value(operands[1],
                                              (a_loc_base.clone(), a_loc_offs));
                loaded_val1.join(loaded_val2)
            },
            _ => {
                error!("unexpected op_type `{}'", op_type);
                panic!()
            } // This should never be called on something TODO: handle with .expect()
            // that is not an OpStore or Comment("mem")
        }
    }

    /// Computes an abstract value for a given node
    /// uninitialized value for Comment("<reg>")
    /// uninitialized value for Comment("mem")
    /// c for Constant(c)
    /// value at a-loc (+ offset) for Load from a-loc (+ offset)
    /// stored value for Store
    /// result of (arithmetic) operation
    fn compute_abstract_value(&self,
                              node: <<RFn as RFunction>::SSA as SSA>::ValueRef)
    -> StridedInterval_u
    {
        let node_data = self.ssa.get_node_data(&node).expect("No node data.");
        let op_type = node_data.nt;
        let operands = self.ssa.get_operands(&node);
        debug!("compute_concrete_value({:?})", node);
        debug!("\tcalc: {:?}", self.print_node_as_comp(node));
        debug!("\toperands: {:?}", operands);
        match op_type {
            NodeType::Comment(ref c) if is_register(c) => {
                StridedInterval_u::Undefined
            },
            NodeType::Op(MOpcode::OpConst(c)) => {
                StridedInterval_u::from_const(c as i64)
            },
            //NodeType::Comment(ref c) if is_base_pointer(c) => {},
            //NodeType::Comment(ref c) if is_gen_purpose_reg(c) => {},
            NodeType::Op(MOpcode::OpStore) => {
                self.compute_abstract_value(operands[2])
            },
            NodeType::Op(MOpcode::OpLoad) => {
                let (a_loc_base, a_loc_offs) =
                    self.compute_a_loc(operands[1])
                    .expect("No base a-loc found");
                self.compute_loaded_value(operands[0], (a_loc_base, a_loc_offs))
            },
            //TODO use process_op()
            NodeType::Op(MOpcode::OpAdd) => {
                self.compute_abstract_value(operands[0]) +
                    self.compute_abstract_value(operands[1])
            },
            NodeType::Op(MOpcode::OpSub) => {
                self.compute_abstract_value(operands[0]) -
                    self.compute_abstract_value(operands[1])
            },
            NodeType::Op(MOpcode::OpMul) => {
                self.compute_abstract_value(operands[0]) *
                    self.compute_abstract_value(operands[1])
            },
            NodeType::Op(MOpcode::OpDiv) => {
                self.compute_abstract_value(operands[0]) /
                    self.compute_abstract_value(operands[1])
            },
            //NodeType::Op(opcode) => {
            //    match opcode {}
            //},
            _ => {
                warn!("Fallthrough");
                StridedInterval_u::Undefined
            }, // FIXME
        }
    }
 
    /// Computes either underlying register or memory region plus offset
    /// takes a node as argument
    // TODO seems conceptually broken, a-loc already includes offset
    // second ret value same as compute_abstract_value?
    // currently returns stuff with ambigous meaning:
    //      on OpStore returns the content of the a-loc
    //      instead of a-loc offset (which is already included in the a-loc)
    fn compute_a_loc(&self, node: <<RFn as RFunction>::SSA as SSA>::ValueRef)
    -> Option<(A_Loc<<<RFn as RFunction>::SSA as SSA>::ValueRef>, i64)>
    {
        debug!("compute_a_loc({:?})", node);
        debug!("\tcalc: {:?}", self.print_node_as_comp(node));
        let node_data = self.ssa.get_node_data(&node).expect("No node data.");
        let ValueType::Integer {width} = node_data.vt;
        let op_type = node_data.nt;
        let operands = self.ssa.get_operands(&node);
        match op_type {
            //NodeType::Comment(ref c) if is_stack_pointer(c) => {
            // just found a stack pointer - nothing special
            // TODO store information about stack in FnAnalyzer
            // TODO what about use of basepointer?
            //    let vt = node_data.vt;
            //    debug!("Found Local a-loc - rsp (compute_a_loc), offset: 0");
            //    Some((A_Loc {
            //        addr: AbstractAddress::MemAddr {
            //            region: self.mem_reg_local.clone(),
            //            offset: 0, //TODO other initial value?
            //        },
            //        size: Some(width as i64),
            //    }, 0))
            //},
            //NodeType::Comment(ref c) if is_base_pointer(c) => {
            NodeType::Comment(ref c) if is_register(c) => {
                Some((A_Loc {
                    addr: AbstractAddress::Reg {
                        reg_name: c.clone(),
                    },
                    size: Some(width as i64),
                }, 0))
            },
            NodeType::Comment(ref c) if c.eq("mem") => {
                // TODO Probably not what we want, but easier to detect in the end
                debug!("Found Global a-loc (compute_a_loc), offs: 0");
                Some((A_Loc {
                    addr: AbstractAddress::MemAddr {
                        // TODO Have *one* global memregion
                        region: MemRegion::new(MemRegionType::Global),
                        // TODO 0 is probably not the right offset
                        offset: 0,
                    },
                    size: Some(width as i64),
                }, 0))
            },
            NodeType::Comment(c) => None, // TODO
            NodeType::Op(MOpcode::OpStore) => { //TODO
                // should return set of all a-loc that have been written to
                // up to 'now'
                // value that will be stored in the a-loc
                let mem_state   = operands[0];
                let target_node = operands[1];
                let value_node  = operands[2];
                let value = self.compute_abstract_value(value_node).as_const();
                // TODO we want to get value of a-loc, too
                if let Some((a_loc_base, a_loc_offs)) =
                    self.compute_a_loc(target_node) {
                        if let A_Loc{
                            addr: AbstractAddress::Reg{ reg_name: reg_name},
                            ..} = a_loc_base {
                                let mem_reg = if reg_name.eq("rip") { // what about "mem"?
                                    MemRegion{region_type: MemRegionType::Global}
                                } else {
                                    self.mem_reg_local.clone()
                                };
                                Some ((A_Loc {
                                    addr: AbstractAddress::MemAddr {
                                        region: mem_reg,
                                        offset: a_loc_offs,
                                    },
                                    size: Some(width as i64),
                                }, value))
                            } else {None}
                } else {None}
            },
            NodeType::Op(MOpcode::OpLoad) => {None}, // TODO
            NodeType::Op(m_opcode) => {
                if operands.len() >= 2 {
                    let update = self.compute_abstract_value(operands[1]).as_const();
                    if let Some((a_loc_base, a_loc_offs)) =
                        self.compute_a_loc(self.ssa.lhs(&node)) {
                            Some((a_loc_base,
                                  perform_op(m_opcode, vec![a_loc_offs, update]))) //TODO
                    } else {None}
                } else if operands.len() >= 1 {
                    let update = self.compute_abstract_value(operands[0]).as_const();
                    if let Some((a_loc_base, a_loc_offs)) =
                        self.compute_a_loc(self.ssa.lhs(&node)) {
                            Some((a_loc_base,
                                  perform_op(m_opcode, vec![a_loc_offs, update]))) //TODO
                    } else {None}
                } else {None}
            },
            NodeType::Phi => {
                // TODO will this code ever be executed?
                // For the case both a-locs are the same, simply return
                let a_loc_off_a = self.compute_a_loc(operands[0]);
                let a_loc_off_b = self.compute_a_loc(operands[1]);
                if a_loc_off_a == a_loc_off_b {
                    a_loc_off_a
                } else {
                    warn!("don't know which a-loc to return");
                    None // TODO what to do otherwise?
                }
            },
            NodeType::Undefined => None,
        }
    }

    /// Analyze a single function.
    //TODO: rename -> analyze ?
    pub fn analyze_rfn(mut self)
        -> AbstractStore<<<RFn as RFunction>::SSA as SSA>::ValueRef>
    {
        // mem region for function
        info!("analyzing function");
        for node in self.ssa.nodes() {
            debug!("analyzing node: {:?}", node);
            if let Ok(node_data) = self.ssa.get_node_data(&node) {
                debug!("\t\tnode data: {:?}", node_data);
                debug!("\t\tcalc: {}", self.print_node_as_comp (node));
                debug!("\t\tvalue: {}", self.compute_abstract_value (node));
            }
            if self.ssa.is_expr (&node) {
                debug!("\t\tis expr: {:?}", node);
                //debug!("\t\toperands: {:?}", self.ssa.get_operands(&node));
                //debug!("\t\tnode data: {:?}", self.ssa.get_node_data(&node));
                debug!("\t\tcalc: {}", self.print_node_as_comp (node));
                debug!("\t\tvalue: {}", self.compute_abstract_value (node));
                //debug!("\t\t#operands ({}):", self.ssa.get_operands(&node).len());
                //for operand in self.ssa.get_operands (&node) {
                //    debug!("\t\t\t{:?}", self.ssa.get_node_data(&operand));
                //    //debug!("\t\t\tinvolved regs:");
                //    //for reg in involved_registers (self.ssa, operand) {
                //    //    debug!("\t\t\t\t{:?}", reg);
                //    //}
                //}

                { //compute a-loc for SSA node
                    let a_loc = A_Loc {
                        addr: AbstractAddress::new_ssa_node(node),
                        size: None, // FIXME
                    };
                    let content = self.compute_abstract_value(node);
                    debug!("Computed concrete value: {}", content);
                    self.a_store_fn.store.insert(a_loc.clone(), content);
                }
                debug!("Computet a-loc for SSA-node");

                //compute a-loc/check for existing a-loc
                if let Some((a_loc_base, a_loc_offs)) =
                    self.compute_a_loc(node) {
                        debug!("Computed a-loc");
                        let op_type = self.ssa.get_node_data(&node)
                                          .expect("No node data.").nt;
                        //debug!("{:?}", op_type);
                        debug!("calc: {}", self.print_node_as_comp (node));
                        //compute value-set
                        let content = self.compute_abstract_value(node);
                        debug!("a-loc: {}", a_loc_base);
                        debug!("\tcontent: {}", content);
                        self.a_store_fn.update (a_loc_base.clone(), content.clone());
                        //update a-loc -> value-set
                }
            }
        };
        self.a_store_fn
    }
}

/// A Value Set Analyzer (VSAnalyzer)
/// This analyzes access patterns in memory
// make generic over architecture
pub trait ValueSetAnalyzer {
    type N: Hash + Eq + Clone;
    fn analyze_value_sets_ssa (&self) -> AbstractStore<Self::N>;
    //fn analyze_value_sets_esil (&'a mut self) -> AbstractStore<N>;
}

impl<'a, F: RFunction + Clone> ValueSetAnalyzer for RadecoModule<'a, F>
where <F as RFunction>::SSA: Clone
{
    type N = <<F as RFunction>::SSA as SSA>::ValueRef;
    fn analyze_value_sets_ssa (&self) -> AbstractStore<Self::N> {
        let mut a_store = AbstractStore::new();
        let mem_reg_global: MemRegion = MemRegion::new(MemRegionType::Global);
        let fkns = self.functions.iter();
        for (ref addr, rfn) in fkns {
            if (!rfn.fn_name().eq("sym.main")) & (!rfn.fn_name().eq("main")) {
                continue;
            }
            let fn_analyzer = FnAnalyzer::from((*rfn).clone());
            let mut a_store_fn = fn_analyzer.analyze_rfn();
            a_store.merge (&mut a_store_fn);
        }
        println!("Returning Abstract Store:");
        for (a_loc, strid_interv) in &a_store.store {
            if let A_Loc{addr: AbstractAddress::Node{node: node}, ..} = *a_loc {
                continue;
            };
            println!("{:?}", a_loc);
            println!("Strided Interval: {}", strid_interv);
        }
        a_store
    }

    //fn analyze_value_sets_esil (&'a mut self) -> AbstractStore {
    //}
}

#[cfg(test)]
mod vsa {
    use super::*;
    use frontend::containers::RadecoModule;
    use frontend::source::FileSource;

    #[test]
    #[ignore]
    // Disable it temporarily.
    fn exist_ssa() {
        let mut fsource = FileSource::open(Some("./test_files/ct1_sccp_ex/ct1_sccp_ex"));
        let rmod = RadecoModule::from(&mut fsource);
    
        let a_store = rmod.analyze_value_sets_ssa ();
    }
}
