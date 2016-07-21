//! Defines valid types.

use std::fmt;

use petgraph::graph::{Graph, NodeIndex};

#[derive(Clone, Debug)]
pub enum RType {
    Overdefined,
    Reg8,
    Reg16,
    Reg32,
    Reg64,
    Num8,
    Num16,
    Num32,
    Num64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Bool,
    Undefined,
    Ptr(Box<RType>),
    Code,
}

impl fmt::Display for RType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let t = if let RType::Ptr(ref ty) = *self {
            format!("{}*", ty)
        } else {
            String::new()
        };

        let s = match *self {
            RType::Overdefined => "overdefined",
            RType::Reg8 => "reg8",
            RType::Reg16 => "reg16",
            RType::Reg32 => "reg32",
            RType::Reg64 => "reg64",
            RType::Num8 => "n8",
            RType::Num16 => "n16",
            RType::Num32 => "n32",
            RType::Num64 => "n64",
            RType::UInt8 => "u8",
            RType::UInt16 => "u16",
            RType::UInt32 => "u32",
            RType::UInt64 => "u64",
            RType::Int8 => "i8",
            RType::Int16 => "i16",
            RType::Int32 => "i32",
            RType::Int64 => "i64",
            RType::Bool => "bool",
            RType::Undefined => "underdefined",
            RType::Ptr(_) => t.as_ref(),
            RType::Code => "code",
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug)]
pub enum ConstraintNode {
    Type(RType),
    TypeVar(usize),
    DisjunctiveJoin,
    ConjunctiveJoin,
    Equality,
    Union,
    Intersect,
    Subtype,
}

#[derive(Clone, Debug)]
pub enum ConstraintEdge {
    EdgeIdx(u8),
}

#[derive(Clone, Debug)]
pub enum SubTypeNode {
    Tmp
}

#[derive(Clone, Debug)]
pub enum SubTypeEdge {
    Tmp
}

#[derive(Clone, Debug)]
pub struct SubTypeSet {
    g: Graph<SubTypeNode, SubTypeEdge>,
}

#[derive(Clone, Debug)]
pub struct ConstraintSet {
    g: Graph<ConstraintNode, ConstraintEdge>,
    type_vars: Vec<NodeIndex>,
    subty: SubTypeSet,
    binding_map: HashMap<usize, RType>,
    upper_bound: HashMap<usize, RType>,
    lower_bound: HashMap<usize, RType>,
}

impl ConstraintSet {
    pub fn new() -> ConstraintSet {
        ConstraintSet {
            g: Graph::new(),
            type_vars: Vec::new(),
            subty: Graph::new(),
            binding_map: HashMap::new(),
            upper_bound: HashMap::new(),
            lower_bound: HashMap::new(),
        }
    }

    // Return the constraints contained by the set
    pub fn constraints(&self) -> ::std::slice::Iter<NodeIndex> {
        unimplemented!()
    }

    pub fn rhs(&self, n: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn lhs(&self, n: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn insert_type_var(&mut self) -> NodeIndex {
        unimplemented!()
    }

    pub fn insert_type(&mut self, ty: RType) -> NodeIndex {
        unimplemented!()
    }

    pub fn disjunctive_join(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn conjunctive_join(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn equal(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn union(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn intersect(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }
}
