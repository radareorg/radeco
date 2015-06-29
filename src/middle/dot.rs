//! Module to parse and write dot files for graphs.

macro_rules! add_strings {
    ( $( $x: expr ),* ) => {
        {
            let mut s = String::new();
            $(
                s = format!("{}{}", s, $x);
             )*
                s
        }
    };
}

pub trait Label {
    fn label(&self) -> String;
    fn name(&self) -> Option<String>;
}

pub trait GraphDot {
    type NodeType: Label;
    type EdgeType: Label + EdgeInfo;
    fn configure(&self) -> String;
    fn nodes(&self) -> Vec<Self::NodeType>;
    fn edges(&self) -> Vec<Self::EdgeType>;
    fn get_node(&self, n: usize) -> Option<&Self::NodeType>;
}

pub trait EdgeInfo {
    fn source(&self) -> usize;
    fn target(&self) -> usize;
}

pub fn emit_dot<T: GraphDot>(g: &T) -> String {
    let mut result = String::new();
    result = add_strings!(result, g.configure());

    // Node configurations
    for node in g.nodes().iter() {
        result = add_strings!(result, node.label());
    }

    // Connect nodes by edges.
    for edge in g.edges().iter() {
        let src_node = g.get_node(edge.source()).unwrap();
        let dst_node = g.get_node(edge.target()).unwrap();
        result = add_strings!(result, src_node.name().unwrap(), " -> ",
                              dst_node.name().unwrap(), edge.label());
    }

    add_strings!(result, "\n}")
}
