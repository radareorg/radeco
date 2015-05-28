// TODO: Add License information.

//! ESIL (Evaluable Strings Intermediate Language) is the IL used by radare2.
//! For a complete documentation of ESIL please checkout:
//!  https://github.com/radare/radare2/wiki/ESIL
//! This module is used to parse ESIL strings and convert them into the IR.

use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub enum Arity {
    Zero,
    Unary,
    Binary,
    Ternary,
}

impl Arity {
    pub fn n(self) -> u8 {
        match self {
            Arity::Zero    => 0,
            Arity::Unary   => 1,
            Arity::Binary  => 2,
            Arity::Ternary => 3,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Operator<'a> {
    op: &'a str,
    arity: Arity,
}

impl<'a> Operator<'a> {
    pub fn new(op: &str, n: Arity) -> Operator {
        Operator { op: op, arity: n }
    }

    pub fn nop() -> Operator<'a> {
        Operator { op: "nop", arity: Arity::Zero }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Location {
    Memory,
    Register,
    Constant,
    Temporary,
    Unknown,
    Null,
}

#[derive(Debug, Clone)]
/// Value is used to represent operands to an operator in a statement.
pub struct Value {
    /// Name
    name: String,
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

impl<'a> Value {
    pub fn new(name: &'a str, size: u8, location: Location, value: i64, typeset: u32) -> Value {
        Value {
            name: name.to_string().clone(),
            size: size,
            location: location,
            value: value,
            typeset: typeset,
        }
    }

    pub fn null() -> Value {
        Value {
            name: String::from(""),
            size: 0,
            location: Location::Null,
            value: 0,
            typeset: 0,
        }
    }

    pub fn new_tmp(i: u64) -> Value {
        Value {
            name: format!("tmp_{:x}", i),
            size: 0,
            location: Location::Temporary,
            value: 0,
            typeset: 0,
        }
    }
}
    

#[derive(Debug, Clone)]
pub struct Instruction<'a> {
    opcode: Operator<'a>,
    dest: Value,
    operand_1: Value,
    operand_2: Value,
}

impl<'a> Instruction<'a> {
    pub fn new(opcode: Operator<'a>, dest: Value, op1: Value, op2: Value) -> Instruction<'a> {
        Instruction {
            opcode: opcode,
            dest: dest,
            operand_1: op1,
            operand_2: op2,
        }
    }
    pub fn to_string(&self) -> String {
        if self.opcode.op == "=" {
            format!("{} {} {}", self.operand_1.name, self.opcode.op, self.operand_2.name)
        } else {
            format!("{} {} {} {} {}", self.dest.name, "=", self.operand_1.name, self.opcode.op, self.operand_2.name)
        }
    }
}

macro_rules! hash {
    ( $( ($x:expr, $y:expr) ),* ) => {
        {
            let mut temp_hash = HashMap::new();
            $(
                temp_hash.insert($x, $y);
             )*
            temp_hash
        }
    };
}

fn init_opset() -> HashMap<&'static str, Operator<'static>> {
    // Make a map from esil string to struct Operator.
    // (operator: &str, arity: Arity).
    // Possible Operatortimization:  Move to compile-time generation ?
    hash![
        ("==" , Operator::new("==", Arity::Binary)),
        ("<"  , Operator::new("<" , Arity::Binary)),
        (">"  , Operator::new(">" , Arity::Binary)),
        ("<=" , Operator::new("<=", Arity::Binary)),
        (">=" , Operator::new(">=", Arity::Binary)),
        ("<<" , Operator::new("<<", Arity::Binary)),
        (">>" , Operator::new(">>", Arity::Binary)),
        ("&"  , Operator::new("&" , Arity::Binary)),
        ("|"  , Operator::new("|" , Arity::Binary)),
        ("="  , Operator::new("=" , Arity::Binary)),
        ("*"  , Operator::new("*" , Arity::Binary)),
        ("^"  , Operator::new("^" , Arity::Binary)),
        ("+"  , Operator::new("+" , Arity::Binary)),
        ("-"  , Operator::new("-" , Arity::Binary)),
        ("/"  , Operator::new("/" , Arity::Binary)),
        ("%"  , Operator::new("%" , Arity::Binary)),
        ("?{" , Operator::new("?{", Arity::Unary)),
        ("!"  , Operator::new("!" , Arity::Unary)),
        ("--" , Operator::new("--", Arity::Unary)),
        ("++" , Operator::new("++", Arity::Unary)),
        ("}"  , Operator::new("}" , Arity::Zero))
    ]
}

fn init_regset() -> HashMap<&'static str, u8> {
    // Use from sdb later, probably a better option.
    hash![
        ("rax", 64),
        ("rbx", 64),
        ("rcx", 64),
        ("rdx", 64),
        ("rsp", 64),
        ("rbp", 64),
        ("rsi", 64),
        ("rdi", 64),
        ("rip", 64)
    ]
}

pub struct Parser<'a> {
    stack: Vec<Value>,
    insts: Vec<Instruction<'a>>,
    opset: HashMap<&'a str, Operator<'a>>,
    regset: HashMap<&'a str, u8>,
    tmp_index: u64,
}

impl<'a> Parser<'a> {
    pub fn new() -> Parser<'a> {
        Parser { 
            stack: Vec::new(),
            insts: Vec::new(),
            opset: init_opset(),
            regset: init_regset(),
            tmp_index: 0,
        }
    }

    fn get_tmp_register(&mut self) -> Value {
        self.tmp_index += 1;
        Value::new_tmp(self.tmp_index)
    }

    fn add_inst(&mut self, op: Operator<'a>) {
        let op2 = match self.stack.pop() {
            Some(ele) => ele,
            None => return,
        };
        let mut op1 = Value::null();
        if op.arity.n() == 2 {
            op1 = match self.stack.pop() {
                Some(ele) => ele,
                None => return,
            };
        }
        let dst: Value = match op.op {
            "=" => Value::null(),
            _ => self.get_tmp_register(),
        };
        self.insts.push(Instruction::new(op, dst.clone(), op2, op1));
        self.stack.push(dst);
    }

    pub fn parse(&mut self, esil: &'a str) {
        for token in esil.split(',') {
            let op = match self.opset.get(token) {
                Some(op) => op.clone(),
                None => Operator::nop(),
            };

            if op.op != "nop" {
                self.add_inst(op);
                continue;
            }

            if !token.contains('=') {
                // Treat it as a operand.
                let mut val_type: Location;
                let mut val: i64 = 0;
                // Change this default based on arch.
                let mut size: u8 = 64;
                
                if let Some(s) = self.regset.get(token) {
                    val_type = Location::Register;
                    // For now, reg is just a u8.
                    size = *s; 
                } else if token.contains("temp") {
                    val_type = Location::Temporary;
                } else if let Ok(v) = token.parse::<i64>() {
                    val_type = Location::Constant;
                    val = v;
                } else {
                    val_type = Location::Unknown;
                }

                let v = Value::new(token, size, val_type, val, 0);
                self.stack.push(v);
                continue;
            }

            // Expand the 'composite' operators to 'basic' ones.
            for t in token.split_terminator('=') {
                let o = match self.opset.get(t) {
                    Some(op) => op.clone(),
                    None => Operator::nop(), 
                };
                if o.op == "nop" {
                    // Return error here instead.
                    return;
                }
                let dst = self.stack.last().unwrap().clone();
                self.add_inst(o);
                self.stack.push(dst);
                self.add_inst(Operator::new("=", Arity::Binary));
            }
        }
    }

    // Should actually return an iter instead.
    pub fn emit_insts(&self) -> Vec<Instruction<'a>> {
        (self).insts.clone()
    }
}

#[test]
fn testing() {
	parse(&mut "0,0x204db1,rip,+,[1],==,%z,zf,=,%b8,cf,=,%p,pf,=,%s,sf,=".to_string());
}
