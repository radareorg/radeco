//! Defines valid types.

use std::fmt;
use std::collections::HashMap;

use petgraph::graph::{EdgeIndex, Graph, NodeIndex};
use petgraph::EdgeDirection;

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
    Ptr,
    TypeVar,
    DisjunctiveJoin,
    ConjunctiveJoin,
    Union,
    Intersect,
}

const EQUAL_EDGE: ConstraintEdge = ConstraintEdge::Equal;
const SUBTYPE_EDGE: ConstraintEdge = ConstraintEdge::SubType;

#[derive(Clone, Debug)]
pub enum ConstraintEdge {
    EdgeIdx(u8),
    SubType,
    Equal,
    Ptr,
}

#[derive(Clone, Debug)]
pub enum SubTypeNode {
    // Index in the original constaints set.
    OrigIdx(NodeIndex),
}

#[derive(Clone, Debug)]
pub enum SubTypeEdge {
    SubType,
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
    // S<:
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

    pub fn operands(&self, n: &NodeIndex) -> Vec<NodeIndex> {
        let mut result = Vec::new();
        for (n, e) in self.g.edges_directed(*n, EdgeDirection::Outgoing) {
            if let ConstraintEdge::EdgeIdx(i) = *e {
                result.push((i, n));
            }
        }
        result.sort_by(|a, b| a.0.cmp(&b.0));
        result.iter().map(|a| a.1).collect()
    }

    pub fn insert_type_var(&mut self, named: Option<String>) -> NodeIndex {
        let var = self.g.add_node(ConstraintNode::TypeVar);
        let name = named.unwrap_or_else(|| format!("var_{}", self.type_vars.keys().len()));
        self.type_vars.insert(name, var);
        var
    }

    pub fn insert_abstract_type(&mut self) -> NodeIndex {
        self.g.add_node(ConstraintNode::AbstractType)
    }

    pub fn insert_base_type(&mut self, ty: RType) -> NodeIndex {
        self.g.add_node(ConstraintNode::Type(ty))
    }

    pub fn subtype(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) {
        self.g.update_edge(*lhs, *rhs, SUBTYPE_EDGE);
    }

    pub fn supertype(&mut self, lhs: &NodeIndex, rhs: &NodeIndex) {
        self.subtype(rhs, lhs)
    }

    pub fn disjunctive_join(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        let disjunction = self.g.add_node(ConstraintNode::DisjunctiveJoin);
        for (i, idx) in operands.iter().enumerate() {
            self.g.update_edge(disjunction, *idx, ConstraintEdge::EdgeIdx(i as u8));
        }
        disjunction
    }

    pub fn conjunctive_join(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        let conjunction = self.g.add_node(ConstraintNode::ConjunctiveJoin);
        for (i, idx) in operands.iter().enumerate() {
            self.g.update_edge(conjunction, *idx, ConstraintEdge::EdgeIdx(i as u8));
        }
        conjunction
    }

    pub fn equal(&mut self, operands: &[NodeIndex; 2]) {
        self.g.update_edge(operands[0], operands[1], EQUAL_EDGE);
    }

    pub fn union(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        let union = self.g.add_node(ConstraintNode::Union);
        for (i, idx) in operands.iter().enumerate() {
            self.g.update_edge(union, *idx, ConstraintEdge::EdgeIdx(i as u8));
        }
        union
    }

    pub fn intersect(&mut self, operands: &[NodeIndex]) -> NodeIndex {
        let intersection = self.g.add_node(ConstraintNode::Intersect);
        for (i, idx) in operands.iter().enumerate() {
            self.g.update_edge(intersection, *idx, ConstraintEdge::EdgeIdx(i as u8));
        }
        intersection
    }

    pub fn solve(&mut self) {
        unimplemented!()
    }

    fn inner_type(&self, ptr_node: NodeIndex) -> NodeIndex {
        self.g
            .edges_directed(ptr_node, EdgeDirection::Outgoing)
            .find(|x| {
                match *x.1 {
                    ConstraintEdge::Ptr => true,
                    _ => false,
                }
            })
            .expect("Inner type of `Ptr` cannot be `None`")
            .0
    }

    // This function decomposes the first node and adds the appropriate edges to
    // the other node.
    fn decompose_constraint_node(&mut self, lhs: NodeIndex, rhs: NodeIndex) {
        let rhs_is_ptr = match self.g[rhs] {
            ConstraintNode::Intersect => {
                let operands = self.operands(&rhs);
                for op in &operands {
                    self.g.update_edge(lhs, *op, ConstraintEdge::SubType);
                }
                false
            }
            ConstraintNode::Ptr => true,
            _ => false,
        };

        let lhs_is_ptr = match self.g[lhs] {
            ConstraintNode::Union => {
                let lhs_operands = self.operands(&lhs);
                let mut subtype_relations = Vec::new();
                let mut edges = self.g.neighbors_directed(lhs, EdgeDirection::Incoming).detach();
                while let Some(edge) = edges.next_edge(&self.g) {
                    if let ConstraintEdge::SubType = self.g[edge] {
                        let (_, target) = self.g.edge_endpoints(edge).expect("");
                        self.g.remove_edge(edge);
                        subtype_relations.push(target);
                    }
                }
                for rhs_op in subtype_relations {
                    for lhs_op in &lhs_operands {
                        self.g.update_edge(*lhs_op, rhs_op, ConstraintEdge::SubType);
                    }
                }
                false
            }
            ConstraintNode::Ptr => true,
            _ => false,
        };

        if lhs_is_ptr && rhs_is_ptr {
            let lhs_inner = self.inner_type(lhs);
            let rhs_inner = self.inner_type(rhs);
            self.g.update_edge(lhs_inner, rhs_inner, ConstraintEdge::SubType);
        }
    }

    fn decompose_subtype_relation(&mut self, subtype_edge: EdgeIndex) {
        // According to section 6.3.2 - Decomposition Rules.
        let (lhs, rhs) = self.g.edge_endpoints(subtype_edge).expect("This cannot be None");
        self.decompose_constraint_node(lhs, rhs);
    }

    fn meet(&mut self, operands: &[RType]) -> RType {
        unimplemented!()
    }

    fn join(&mut self, operands: &[RType]) -> RType {
        unimplemented!()
    }
}
