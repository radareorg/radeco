/*
	In a first pass we identify all points where phi nodes have to be generated,
	which is every position that is target of jump. Then we determine the number
	of untouched elements in this basic block at the bottom of the stack.

 	    ,- - - - -
	- > ` > - > - >

	===| |=========
	BB1   BB2

	In the second pass we…

*/

enum EsilOp {
	PushInt(u64),
    BinaryBoolean,
    BinaryScalar,
    BinaryVoid,
	If,
	IfEnd,
    Pop,
	Todo,
	Goto,
	Break,
	Clear,
	Dup
}

struct AbstractStack

fn op_stack_delta(op: &EsilOp) {
	match op {
		PushInt(ref x) => 1,
		BinaryBoolean => -1,
		BinaryScalar => -1,
		BinaryVoid => -2,
		If => -1,
		IfEnd => 0,
		Pop => -1,
		Todo => 0,
		Goto => -1,
		Break => 0,
		Clear =>
	}
}

pub fn analyze_esil(input: &mut String) {
	
}

fn analyze_ops(ops: &[EsilOp]) {
	let mut needphi: [bool; ops.len()];
	for op in ops
}

#[test]
fn testing() {
	analyze("0,0x204db1,rip,+,[1],==,%z,zf,=,%b8,cf,=,%p,pf,=,%s,sf,=".to_string());
}
