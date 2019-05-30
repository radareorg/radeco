//! This module offers structs and traits for valueset analysis as inctroduced
//! in "Analyzing Memory Access in x86 Executables" by Gogul Balakrishnan and
//! Thomas Reps
//! It offers datastructures specific to memory access

use std::collections::HashMap;
use std::hash::Hash;
use std::fmt::Debug;
use std::fmt;

use petgraph::graph::NodeIndex;

use super::{AbstractValue,StridedInterval_u};

use middle::ssa::ssa_traits;
use middle::ssa::ssastorage::{SSAStorage};

/// Type of a memory region
#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum MemRegionType {
    /// Global memory region
    Global,
    /// Function-local memory region (each function has its own local memory
    /// region on the stack)
    Local,
    /// Dynamic memory region - allocated at runtime on the heap
    Dynamic,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct MemRegion
{
    pub region_type: MemRegionType,
    //a_locs: HashMap<A_Loc, u64>,
    // Maybe some identifyer and String representation
}

impl MemRegion {
    pub fn new(r_type: MemRegionType) -> MemRegion {
        MemRegion {
            region_type: r_type,
        }
    }
}

impl fmt::Display for MemRegion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.region_type {
            MemRegionType::Global
                => write!(f, "Global Memory Region"),
            MemRegionType::Local
                => write!(f, "Local Memory Region", ),
            MemRegionType::Dynamic
                => write!(f, "Dynamic Memory Region"),
        }
    }
}

// Do not use fix sized ints, but generic types?
#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum AbstractAddress<T> {
    MemAddr {
        region: MemRegion, // probably want ref
        offset: i64,
    },
    Reg {
        reg_name: String,
    },
    Node { // SSA Node
        node: T,
    },
}

impl<T> AbstractAddress<T> {
    pub fn new_ssa_node(node: T)
        -> AbstractAddress<T> {
        AbstractAddress::Node {
            node: node,
        }
    }
}

impl<T> fmt::Display for AbstractAddress<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AbstractAddress::MemAddr {ref region, offset}
                => write!(f, "Memory Address: {}, {}", region, offset),
            &AbstractAddress::Reg {ref reg_name}
                => write!(f, "Register: {}", reg_name),
            &AbstractAddress::Node {ref node}
                => write!(f, "SSA-Node"),
        }
    }
}


// Do not use fix sized ints, but generic types?
/// "a-loc" (abstract location)
#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct A_Loc<T> {
    //region: MemRegion,
    //// Maybe use UIntRange instead
    //offset: i64,
    //pub addr: AbstractAddress,
    pub addr: AbstractAddress<T>,
    pub size: Option<i64>,
}

impl<T> A_Loc<T> {
    fn new(region: MemRegion, offset: i64, size: Option<i64>) -> A_Loc<T> {
        A_Loc {
            addr: AbstractAddress::MemAddr {
                region: region,
                offset: offset,
            },
            size: size,
        }
    }

    //fn from_node<A: SSA>(ssa: &A, node: A::ValueRef) -> A_Loc {
    //}
}

impl<T> fmt::Display for A_Loc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.size {
            Some(size)
                => write!(f, "a-loc: {} ({})", self.addr, size),
            None
                => write!(f, "a-loc: {} (unknown size)", self.addr),
        }
    }
}

//TODO
//pub enum AbstractValue {
//    StridedInterval(StridedInterval),
//    UnknownInitialV{comment: String},
//    ComposedValue(Op, Operators),
//}

/// An abstract store
/// This is a map from an a-loc to a value set
// make generic over architecture
#[derive(Debug)]
pub struct AbstractStore<T>
where T: Hash + Eq + Clone
{
    //pub store: HashMap<A_Loc<T>, AbstractValue>,
    pub store: HashMap<A_Loc<T>, StridedInterval_u>,
    //// alternatively:
    //local: HashMap<A_Loc, u64>,
    //dynamic: HashMap<A_Loc, u64>,
    //global: HashMap<A_Loc, u64>,
}

impl<T> AbstractStore<T>
where T: Hash + Eq + Clone + Debug
{
    pub fn new() -> AbstractStore<T> {
        AbstractStore {
            store : HashMap::new(),
            //local : HashMap::new(),
            //dynamic : HashMap::new(),
            //global : HashMap::new(), // a-locs -> ValueSet
        }
    }

    /// Update the abstract store with an a-loc and a strided interval
    /// if the a-loc already exists in the abstract store,
    /// join the strided intervals
    /// otherwise simply insert a-loc -> strided interval
    //pub fn update(&mut self, a_loc: A_Loc<T>, strid_interv: AbstractValue) {
    pub fn update(&mut self, a_loc: A_Loc<T>, strid_interv: StridedInterval_u) {
        //println!("updating:");
        //println!("\t{:?}:", a_loc);
        //println!("\twith: {}", strid_interv);
        //let strid_interv_up: AbstractValue;
        let strid_interv_up: StridedInterval_u;
        if let Some(prev) = self.store.get(&a_loc.clone()) {
            //println!("\tAlready got entry for this a-loc");
            //println!("\tprev val: {}", prev);
            strid_interv_up = prev.join(strid_interv);
            //println!("\tnow: {}", strid_interv_up);
        } else {
            strid_interv_up = strid_interv;
        }
        self.store.insert (a_loc.clone(), strid_interv_up);
    }

    pub fn merge(&mut self, a_store: &mut AbstractStore<T>) {
        for (a_loc, strid_interv) in a_store.store.drain() {
            //TODO handle case when aloc already exists in both
            self.store.insert(a_loc, strid_interv);
        }
    }
}
