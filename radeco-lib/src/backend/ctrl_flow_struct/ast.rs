// B: basic block
// C: condition
// V: variable
#[derive(Debug, Eq, PartialEq)]
pub enum AstNode<B, C, V> {
    BasicBlock(B),
    Seq(Vec<AstNode<B, C, V>>),
    Cond(C, Box<AstNode<B, C, V>>, Option<Box<AstNode<B, C, V>>>),
    Loop(LoopType<C>, Box<AstNode<B, C, V>>),
    Break,
    Switch(V, Vec<(ValueSet, AstNode<B, C, V>)>, Box<AstNode<B, C, V>>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum LoopType<C> {
    PreChecked(C),
    PostChecked(C),
    Endless,
}

pub type ValueSet = (); // XXX

impl<B, C, V> Default for AstNode<B, C, V> {
    /// Creates a no-op node.
    fn default() -> Self {
        AstNode::Seq(Vec::new())
    }
}
