pub mod lang_c;
pub mod scf;

use self::scf::SCFDomain;
use self::scf::SCFNode;

#[derive(Debug)]
pub struct D;
impl<'x> SCFDomain for D {
	type Declaration = String;
	type Expression = String;
	type Statement = String;
	type Node = Box<SCFNode<D>>;
}
