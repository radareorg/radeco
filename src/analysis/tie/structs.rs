//! Defines valid types.

use std::fmt;
use std::collections::HashMap;

use petgraph::graph::{Graph, NodeIndex, EdgeIndex};

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
    Ptr(Box<RType>),
    Code,
    Union(Vec<RType>),
    Intersect(Vec<RType>),
    Undefined,
}

impl fmt::Display for RType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let RType::Ptr(ref ty) = *self {
            write!(f, "{}*", ty)
        } else if let RType::Union(ref tys) = *self {
            let mut s = String::from("union(");
            let len = tys.len();
            for (i, ty) in tys.iter().enumerate() {
                s.push_str(&ty.to_string());
                if i < len - 1 {
                    s.push_str(", ");
                }
            }
            write!(f, "{})", s)
        } else if let RType::Intersect(ref tys) = *self {
            let mut s = String::from("intersect(");
            let len = tys.len();
            for (i, ty) in tys.iter().enumerate() {
                s.push_str(&ty.to_string());
                if i < len - 1 {
                    s.push_str(", ");
                }
            }
            write!(f, "{})", s)
        } else {
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
                RType::Code => "code",
                _ => unreachable!(),
            };
            write!(f, "{}", s)
        }
    }
}

#[derive(Clone, Debug)]
pub enum ConstraintNode {
    Type(RType),
    AbstractType,
    TypeVar,
    DisjunctiveJoin,
    ConjunctiveJoin,
    Union,
    Intersect,
}

#[derive(Clone, Debug)]
pub enum ConstraintEdge {
    EdgeIdx(u8),
    SubType,
    Equal,
}

#[derive(Clone, Debug)]
pub enum SubTypeNode {
    // Index in the original constaints set.
    OrigIdx(NodeIndex),
}

#[derive(Clone, Debug)]
pub enum SubTypeEdge {
    SubType
}

#[derive(Clone, Debug)]
pub struct SubTypeSet {
    g: Graph<SubTypeNode, SubTypeEdge>,
    // Map from NodeIndex in ConstraintSet to NodeIndex in SubTypeSet.
    map: HashMap<NodeIndex, NodeIndex>,
}

impl SubTypeSet {
    pub fn new() -> SubTypeSet {
        SubTypeSet {
            g: Graph::new(),
            map: HashMap::new(),
        }
    }

    // Insert LHS <: RHS
    pub fn insert_relation(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) {
        unimplemented!()
    }

    // For all ( alpha <: S ) return alpha
    pub fn subtypes_of(&self, idx_s: &NodeIndex) -> Vec<NodeIndex> {
        unimplemented!()
    }

    // For all ( T <: beta ) return beta
    pub fn supertypes_of(&self, idx_t: &NodeIndex) -> Vec<NodeIndex> {
        unimplemented!()
    }
}

#[derive(Clone, Debug)]
pub struct ConstraintSet {
    g: Graph<ConstraintNode, ConstraintEdge>,
    type_vars: HashMap<String, NodeIndex>,
    // S <:
    subty: SubTypeSet,
    // S=
    binding_map: HashMap<NodeIndex, RType>,
    // B^
    upper_bound: HashMap<NodeIndex, RType>,
    // B_
    lower_bound: HashMap<NodeIndex, RType>,
}

impl ConstraintSet {
    pub fn new() -> ConstraintSet {
        ConstraintSet {
            g: Graph::new(),
            type_vars: HashMap::new(),
            subty: SubTypeSet::new(),
            binding_map: HashMap::new(),
            upper_bound: HashMap::new(),
            lower_bound: HashMap::new(),
        }
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

    pub fn insert_abstract_type(&mut self) -> NodeIndex {
        unimplemented!()
    }

    pub fn insert_base_type(&mut self, ty: RType) -> NodeIndex {
        unimplemented!()
    }

    pub fn subtype(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) {
        unimplemented!()
    }

    pub fn supertype(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) {
        self.subtype(rhs, lhs)
    }

    pub fn disjunctive_join(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn conjunctive_join(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        unimplemented!()
    }

    pub fn equal(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        unimplemented!()
    }

    pub fn union(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        unimplemented!()
    }

    pub fn intersect(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        unimplemented!()
    }

    pub fn solve(&mut self) {
        unimplemented!()
    }

    fn decompose(&mut self, subtype_edge: EdgeIndex) {
        unimplemented!()
    }

    fn meet(&mut self, operands: &[RType]) -> RType {
        unimplemented!()
    }

    fn join(&mut self, operands: &[RType]) -> RType {
        unimplemented!()
    }
}
