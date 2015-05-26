// TODO: Add License information.

//! ESIL (Evaluable Strings Intermediate Language) is the IL used by radare2.
//! For a complete documentation of ESIL please checkout:
//!   * https://github.com/radare/radare2/wiki/ESIL
//! This module is used to parse ESIL strings and convert them into the IR.

use std::collections::HashMap;

const BINOP: u8 = 2;
const UNOP: u8 = 1;
const NOP: u8 = 0;

#[derive(Debug, Copy, Clone)]
struct Op<'a> {
    op: &'a str,
    operand_count: u8,
}

impl<'a> Op<'a> {
    fn new(op: &str, n: u8) -> Op {
        Op { op: op, operand_count: n }
    }
}

#[derive(Debug, Copy, Clone)]
enum Location {
    Memory,
    Register,
    Constant,
    Temporary,
    Null,
}

#[derive(Debug, Copy, Clone)]
/// Value is used to represent operands to an operator in a statement.
struct Value<'a> {
    /// Name
    name: &'a str,
    /// Size in bits.
    size: u8,
    /// Memory, Register, Constant or Temporary.
    location: Location,
    /// Value if evaluable.
    value: i64,
    // TODO: Convert from u32 to TypeSet.
    // Every value can be considered in terms of typesets rather than fixed
    // types which can then be narrowed down based on the analysis.
    // TypeSet can be implemented simply as a bit-vector.
    typeset: u32,
}

impl<'a> Value<'a> {
    fn new(name: &'a str, size: u8, location: Location, value: i64, typeset: u32) -> Value<'a> {
        Value {
            name: name,
            size: size,
            location: location,
            value: value,
            typeset: typeset,
        }
    }

    fn null() -> Value<'a> {
        Value {
            name: &"",
            size: 0,
            location: Location::Null,
            value: 0,
            typeset: 0,
        }
    }
}
    

#[derive(Debug, Copy, Clone)]
struct Instruction<'a> {
    opcode: Op<'a>,
    dest: Value<'a>,
    operand: [Value<'a>; 2],
}

impl<'a> Instruction<'a> {
    pub fn new(opcode: &'a Op<'a>, dest: Value<'a>, op1: Value<'a>, op2: Value<'a>) -> Instruction<'a> {
        Instruction {
            opcode: opcode.clone(),
            dest: dest,
            operand: [op1, op2],
        }
    }

    pub fn op1(&self) -> &Value {
        &self.operand[0]
    }

    pub fn op2(&self) -> &Value {
        &self.operand[1]
    }

    pub fn dest(&self) -> &Value {
        &self.dest
    }

    pub fn to_string(&self) -> String {
        format!("{} {} {} {} {}", self.dest.name, "=", self.operand[0].name, self.opcode.op, self.operand[1].name)
    }
}

macro_rules! set_ops {
    ( $( ($x:expr, $y:expr) ),* ) => {
        {
            let mut temp_hash = HashMap::new();
            $(
                temp_hash.insert($x, Op::new($x, $y));
             )*
            temp_hash
        }
    };
}

fn make_ops_hash() -> HashMap<&'static str, Op<'static>> {
    // Make a map from esil string to struct Op.
    // (operator: &str, arity: u8).
    // TODO: Move to compile-time generation ?
    set_ops![
        ("==" , BINOP),
        ("<"  , BINOP),
        (">"  , BINOP),
        ("<=" , BINOP),
        (">=" , BINOP),
        ("<<" , BINOP),
        ("<<=", BINOP),
        (">>" , BINOP),
        (">>=", BINOP),
        ("&"  , BINOP),
        ("&=" , BINOP),
        ("|"  , BINOP),
        ("|=" , BINOP),
        ("="  , BINOP),
        ("*"  , BINOP),
        ("*=" , BINOP),
        ("^"  , BINOP),
        ("^=" , BINOP),
        ("+"  , BINOP),
        ("+=" , BINOP),
        ("-"  , BINOP),
        ("-=" , BINOP),
        ("/"  , BINOP),
        ("/=" , BINOP),
        ("%"  , BINOP),
        ("%=" , BINOP),
        ("?{" , UNOP),
        ("!"  , UNOP),
        ("--=", UNOP),
        ("--" , UNOP),
        ("++=", UNOP),
        ("++" , UNOP),
        ("!=" , UNOP),
        ("}"  , NOP)
    ]
}

pub fn parse(esil: &mut String) {
    let ops = make_ops_hash();
    let mut stack: Vec<Value> = Vec::new();
    let mut insts: Vec<Instruction> = Vec::new();
    println!("{}", esil);
    for token in esil.split(',') {
        match ops.get(token) {
            Some(op) => {
                let op2 = match stack.pop() {
                    Some(ele) => ele,
                    None => break,
                };
                let mut op1 = Value::null();
                if op.operand_count == 2 {
                    op1 = match stack.pop() {
                        Some(ele) => ele,
                        None => break,
                    };
                }
                let dst: Value = match op.op {
                    "=" => op2,
                    _ => Value::new("temp_x", 64, Location::Temporary, 0, 0),
                };
                insts.push(Instruction::new(op, dst, op2, op1));
                stack.push(dst);
            },
            None => {
                let v = Value::new(token, 64, Location::Register, 0, 0);
                stack.push(v);
            },
        };
    }
    for inst in insts {
        println!("{}", inst.to_string());
    }
}

#[test]
fn testing() {
	parse(&mut "0,0x204db1,rip,+,[1],==,%z,zf,=,%b8,cf,=,%p,pf,=,%s,sf,=".to_string());
}
