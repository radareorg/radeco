Graph datastructure

InnerGraph

The instructions within a basic block are represented by
    ir::graph::inner::InnerGraph<IndexType, InstructionType>
InnerGraph has a vector with rows like

    [Add, void,
        <index of arg1>,
        <index of arg2>
    ] (when parametrized with InnerEdgeLight)

or

    [Add, <index of first user>,
        [<index of arg1>, <index of prev. user of arg1>, <index of next user>],
        [<index of arg1>, <index of prev. user of arg1>, <index of next user>]
    ] (when parametrized with InnerEdgeLinked)


It can answer queries like args_of(index) and uses_of(index). The light version
scans linearly, the linked version uses the linked lists.

Phi nodes have indices -n..-1
External nodes have indices -m..-n-1

When modifying the argument of an instruction the operand has to be remvoed from
the linked list an when if it was referenced as first user of a certain
instruction, that field has to be updated too. In case of external/phi nodes
that index is not part of InnerGraph. Therefore all modifying instructions get a
reference to an object (AuxQuery) that will give access to this field for
outside instructions.

The outer graph

The top-level graph is based on petgraph parametrized with
    IRNode<Index, Instruction> and IREdge<Index, Instruction>
which are two enums like:

IRNode::BasicBlock
    Note: an edge that points here doesn't point to a specific instruction
    within that basic block
IRNode::Repr
    This node type is used to represent a certain instruction in a basic block.
    And edge going from Repr to BasicBlock will have the index of the 
    instruction selected on its IREdge::ReprToBasicBlock


IREdge::Flow
    Points from one `IR::BasicBlock` to another. Represents control flow. It
    contains a vector of 'n' references to operations in the origin basic block
    that are used by 'n' phi operations in the target basic block.
IREdge::ReprToBlock
    Points from a `IRNode::Repr` to the `IR::BasicBlock` that it belongs to
    (see IRNode::Repr)
IREdge::BlockToRepr
    Points from a `IR::BasicBlock`s to `IRNode::Repr` whose target is used by
    operations in the basic block. Contains a id of the first user in the
    basicblock (or void, see InnerGraph), and the negative id the `IRNode::Repr`
    is known by in the basic block
