//! Module to parse and write dot files for graphs.

// TODO:
//   * Add colors to different elements of the graph.

pub use super::cfg::*;
pub use super::ir::*;

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

pub trait Dot {
    fn to_dot(&self) -> String;
}

impl Dot for CFG {
    fn to_dot(&self) -> String {
        let mut result = String::new();
        result = add_strings!(result, "digraph cfg {\n");
        // Graph configurations
        result = add_strings!(result, "splines=\"true\";\n");
        // Node configurations
        for node in self.g.raw_nodes().iter() {
            result = add_strings!(result, node.weight.to_dot());
        }

        // Connect nodes by edges.
        for edge in self.g.raw_edges().iter() {
            let src_node = self.g.node_weight(edge.source()).unwrap();
            let dst_node = self.g.node_weight(edge.target()).unwrap();
            result = add_strings!(result, src_node.label(), " -> ",
            dst_node.label(), edge.weight.to_dot());
        }
        add_strings!(result, "}")
    }
}

impl Dot for EdgeData {
    fn to_dot(&self) -> String {
        let mut direction = "forward";
        let (color, label) = match self.edge_type {
            EdgeType::True => ("green", "label=T"),
            EdgeType::False => ("red", "label=F"),
            EdgeType::Unconditional => ("black", ""),
        };
        if self.direction == BACKWARD {
            direction = "back";
        }
        add_strings!("[", label, " color=", color, " dir=", direction, "];\n")
    }
}

impl Dot for NodeData {
    fn to_dot(&self) -> String {
        let mut result = String::new();
        let mut color = "black";
        result = add_strings!(result, "<<table border=\"0\" cellborder=\"0\" cellpadding=\"1\">");
        let res = match *self {
            NodeData::Block(ref block) => {
                if !block.reachable {
                    color = "red";
                }
                block.to_dot()
            },
            NodeData::Entry => "<tr><td>Entry</td></tr>".to_string(),
            NodeData::Exit => "<tr><td>Exit</td></tr>".to_string(),
        };
        result = add_strings!(result, res, "</table>>");
        add_strings!(self.label(), "[style=rounded label=", result, " shape=box color=", color,"];\n")
    }
}

impl Dot for BasicBlock {
    fn to_dot(&self) -> String {
        let mut result = String::new();
        for inst in &self.instructions {
            result = add_strings!(result, inst.to_dot());
        }
        result
    }
}

impl Dot for Instruction {
    fn to_dot(&self) -> String {
        format!("<tr><td align=\"left\" cellspacing=\"1\"><font color=\"grey50\"
            point-size=\"9\">0x{:08x}:</font></td><td align=\"left\">{}</td></tr>",
            self.addr, self)
    }
}
