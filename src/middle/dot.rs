//! Module to parse and write dot files for graphs.

macro_rules! add_strings {
    ( $( $x: expr ),* ) => {
        {
            let mut s = String::new();
            $(
                s.push_str(&format!("{}", $x));
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
    fn skip(&self) -> bool { false }
}

pub fn emit_dot<T: GraphDot>(g: &T) -> String {
    let mut result = String::new();
    result = add_strings!(result, g.configure());

    // Node configurations
    let mut i: usize = 0;
    for node in g.nodes().iter() {
        result = add_strings!(result, "n", i, node.label());
        i += 1;
    }

    // Connect nodes by edges.
    for edge in g.edges().iter() {
        if edge.skip() { continue }
        result = add_strings!(result, "n", edge.source(), " -> ",
                              "n", edge.target(), edge.label());
    }

    add_strings!(result, "\n}\n")
}
