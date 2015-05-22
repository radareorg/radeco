use super::D;
use scf::SCFNode;

pub fn serialize(n: &SCFNode<D>) -> String {
	match *n {
		SCFNode::Statement(ref x) =>
			x.clone(),
		SCFNode::Cond{ref cond, ref body, ref alt} =>
			format!("if ({}) {{ {} }} else {{ {} }}", cond, serialize(&*body), serialize(&*alt)),
		_ => "Not implemented".to_string()
	}
}
