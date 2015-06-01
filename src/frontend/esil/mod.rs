// TODO: Add License information.

//! Module to parse ESIL strings and convert them into the IR.
//!
//! ESIL (Evaluable Strings Intermediate Language) is the IL used by radare2.
//!
//! For a complete documentation of ESIL please check 
//!  [wiki](https://github.com/radare/radare2/wiki/ESIL).
//!
//! # Details
//!
//! The `Parser` struct provides methods needed to convert a valid ESIL string
//! into the IR. `Parser::parse()` parses the ESIL string and returns an `Err`
//! if the ESIL string is Invalid.
//!
//! `Parser` also provides `Parser::emit_insts()` to extract the `Instructions` 
//! it generates. Calling `Parser::parse()` several times will add more instructions.
//! 
//! # Example
//!
//! ```
//! use radeco::frontend::esil;
//! let esil = "eax,ebx,^=";
//! let mut p = esil::Parser::new();
//! p.parse(esil);
//! for inst in &p.emit_insts() {
//!     println!("{}", inst.to_string());
//! }
//! ```

use std::collections::HashMap;
use regex::Regex;

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

pub enum Opcode {
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpMod,
    OpAnd,
    OpOr,
    OpXor,
    OpNot,
    OpEq,
    OpCmp,
    OpGt,
    OpLt,
    OpLsl,
    OpLsr,
    OpInc, // Actually composite. Not broken down for now.
    OpDec, // Actually composite. Not broken down for now.
    OpIf,
    OpRef,
    // Width casts.
    OpNarrow,
    OpWiden,
}

impl<'a> Opcode {
    fn to_operator(&self) -> Operator<'a> {
        let (op, arity) = match *self {
            Opcode::OpAdd => ("+", Arity::Binary),
            Opcode::OpSub => ("-", Arity::Binary),
            Opcode::OpMul => ("*", Arity::Binary),
            Opcode::OpDiv => ("/", Arity::Binary),
            Opcode::OpMod => ("%", Arity::Binary),
            Opcode::OpAnd => ("&", Arity::Binary),
            Opcode::OpOr => ("|", Arity::Binary),
            Opcode::OpXor => ("^", Arity::Binary),
            Opcode::OpNot => ("!", Arity::Unary),
            Opcode::OpEq => ("=", Arity::Binary),
            Opcode::OpCmp => ("==", Arity::Binary),
            Opcode::OpGt => (">", Arity::Binary),
            Opcode::OpLt => ("<", Arity::Binary),
            Opcode::OpLsl => ("<<", Arity::Binary),
            Opcode::OpLsr => (">>", Arity::Binary),
            Opcode::OpInc => ("++", Arity::Unary),
            Opcode::OpDec => ("--", Arity::Unary),
            Opcode::OpIf => ("if", Arity::Unary),
            Opcode::OpRef => ("&ref", Arity::Unary),
            Opcode::OpNarrow => ("narrow", Arity::Binary),
            Opcode::OpWiden => ("widen", Arity::Binary),
        };
        Operator::new(op, arity).clone()
    }

    fn format(&self, ins_str: String) -> String {
        // ins_str is of the form: op,dst,dst_size,op1,op1_size,op2,op2_size
        let tokens: Vec<String> = ins_str.split_terminator(',').map(|s| s.to_string()).collect();
        let s = match *self {
            Opcode::OpNot => format!("%{}[:{}] = {}%{}[:{}]", tokens[1], tokens[2], tokens[0], tokens[3], tokens[4]),
            Opcode::OpEq => format!("%{}[:{}] = %{}[:{}]", tokens[1], tokens[2], tokens[3], tokens[4]),
            Opcode::OpInc => format!("%{}[:{}] = %{}[:{}] + 1", tokens[1], tokens[2], tokens[3], tokens[4]),
            Opcode::OpDec => format!("%{}[:{}] = %{}[:{}] - 1", tokens[1], tokens[2], tokens[3], tokens[4]),
            Opcode::OpIf => format!("if (%{}) {{", tokens[3]),
            Opcode::OpRef => format!("%{}[:{}] = {}%{}[:{}]", tokens[1], tokens[2], tokens[0], tokens[3], tokens[4]),
            Opcode::OpNarrow => format!("%{}[:{}] = {}(%{}[:{}], {})", tokens[1], tokens[2], tokens[0], tokens[3], tokens[4], tokens[5]),
            Opcode::OpWiden => format!("%{}[:{}] = {}(%{}[:{}], {})", tokens[1], tokens[2], tokens[0], tokens[3], tokens[4], tokens[5]),
            _ => format!("%{}[:{}] = %{}[:{}] {} %{}[:{}]", tokens[1], tokens[2], tokens[3], tokens[4], tokens[0], tokens[5], tokens[6]),
        };
        s
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

impl Value {
    pub fn new(name: String, size: u8, location: Location, value: i64, typeset: u32) -> Value {
        Value {
            name: name.clone(),
            size: size,
            location: location,
            value: value,
            typeset: typeset,
        }
    }

    pub fn null() -> Value {
        Value::new("".to_string(), 0, Location::Null, 0, 0)
    }

    pub fn tmp(i: u64, size: u8) -> Value {
        Value::new(format!("tmp_{:x}", i), size, Location::Temporary, 0, 0)
    }

    pub fn constant(i: i64) -> Value {
        Value::new(i.to_string(), 64, Location::Constant, i, 0)
    }
}

#[derive(Debug, Clone)]
pub struct Instruction<'a> {
    opcode: Operator<'a>,
    dst: Value,
    operand_1: Value,
    operand_2: Value,
}

impl<'a> Instruction<'a> {
    pub fn new(opcode: Operator<'a>, dst: Value, op1: Value, op2: Value) -> Instruction<'a> {
        Instruction {
            opcode: opcode,
            dst: dst,
            operand_1: op1,
            operand_2: op2,
        }
    }

    pub fn to_string(&self) -> String {
        if self.opcode.arity.n() == 1 {
            format!("{}({}) = {} {}({})", self.dst.name, self.dst.size, 
                    self.opcode.op, self.operand_1.name, self.operand_1.size)
        } else if self.opcode.op == "=" {
            format!("{}({}) = {}({})", self.dst.name, self.dst.size, 
                    self.operand_1.name, self.operand_1.size)
        } else {
            format!("{}({}) = {}({}) {} {}({})", self.dst.name, self.dst.size, self.operand_1.name, 
                    self.operand_1.size, self.opcode.op, self.operand_2.name, self.operand_2.size)
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
    // (operator: &str, op: Operator).
    // Possible Optimization:  Move to compile-time generation ?
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
    default_size: u8,
}

impl<'a> Parser<'a> {
    pub fn new() -> Parser<'a> {
        Parser { 
            stack: Vec::new(),
            insts: Vec::new(),
            opset: init_opset(),
            regset: init_regset(),
            tmp_index: 0,
            // Change this default based on arch.
            default_size: 64,
        }
    }

    fn get_tmp_register(&mut self, mut size: u8) -> Value {
        self.tmp_index += 1;
        if size == 0 {
            size = self.default_size;
        }
        Value::tmp(self.tmp_index, size)
    }

    fn add_widen_inst(&mut self, op: &mut Value, size: u8) {
        if op.size > size {
            return;
        }
        let dst = self.get_tmp_register(size);
        let operator = Opcode::OpWiden.to_operator();
        self.insts.push(Instruction::new(operator, dst.clone(), op.clone(), Value::null()));
        *op = dst;
    }

    fn add_narrow_inst(&mut self, op: &mut Value, size: u8) {
        if op.size < size {
            return;
        }
        let dst = self.get_tmp_register(size);
        let operator = Opcode::OpNarrow.to_operator();
        self.insts.push(Instruction::new(operator, dst.clone(), op.clone(), Value::constant(size as i64)));
        *op = dst;
    }

    fn add_inst(&mut self, op: Operator<'a>) {
        let mut op2 = match self.stack.pop() {
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

        let mut dst_size = op2.size;
        //if op1.size > op2.size {
            //dst_size = op1.size;
            //self.add_widen_inst(&mut op2, op1.size);
        //} else if op1.size < op2.size {
            //dst_size = op2.size;
            //self.add_widen_inst(&mut op1, op2.size);
        //}

        let mut dst: Value;
        if op.op == "=" {
            dst = op2.clone();
            op2 = op1.clone();
            op1 = Value::null();
        } else {
            dst = self.get_tmp_register(dst_size);
        }

        // Add a check to see if dst, op1 and op2 have the same size.
        // If they do not, cast it.

        self.insts.push(Instruction::new(op, dst.clone(), op2, op1));
        self.stack.push(dst);
    }

    pub fn parse(&mut self, esil: &'a str) {
        let expanded_esil: Vec<String> = esil.split(',').map(|x| x.to_string()).collect();
        for token in expanded_esil {
            let op = match self.opset.get(&*token) {
                Some(op) => op.clone(),
                None => Operator::nop(),
            };

            if op.op != "nop" {
                self.add_inst(op);
                continue;
            }

            // If it contains atleast one alpha, it cannot be an operator.
            let re = Regex::new("[a-zA-Z]").unwrap();
            if re.is_match(&*token) {
                let mut val_type = Location::Unknown;
                let mut val: i64 = 0;
                let mut size: u8 = self.default_size;
                if let Some(r) = self.regset.get(&*token) {
                    val_type = Location::Register;
                    // For now, reg is just a u8.
                    size = *r; 
                } else if let Ok(v) = token.parse::<i64>() {
                    val_type = Location::Constant;
                    val = v;
                }
                let v = Value::new(String::from(token), size, val_type, val, 0);
                self.stack.push(v);
                continue;
            }

            // Deal with normal 'composite' instructions.
            if token.char_indices().last().unwrap().1 != ']' {
                let mut dst: Value;
                if let Some(x) = self.stack.last() {
                    dst = x.clone();
                } else {
                    // Return Error.
                    return;
                }
                let re = Regex::new(r"^(.|..)=$").unwrap();
                let t = re.captures(&*token).unwrap().at(1).unwrap_or("");
                if t.len() == 0 {
                    // Return Error.
                    return;
                }
                let op = match self.opset.get(t) {
                    Some(op) => op.clone(),
                    None => return,
                };

                self.add_inst(op);
                self.stack.push(dst);
                self.add_inst(Opcode::OpEq.to_operator());
                continue;
            }

            // Deal with memaccess 'composite' instructions.
            let re = Regex::new(r"^(.|..)?(=)?\[([1248]?)\]$").unwrap();
            let tokens = re.captures(&*token).unwrap();
            let eq = tokens.at(2).unwrap_or("");
            let has_op = tokens.at(1).unwrap_or("");
            let access_size = tokens.at(3).unwrap_or("");
            let access_size = match access_size {
                "" => self.default_size,
                _ => access_size.parse::<u8>().unwrap() * 8,
            };

            self.add_inst(Opcode::OpRef.to_operator());
            // Set the correct size.
            let tmp_dst = self.get_tmp_register(access_size);
            let tmp_dst1 = tmp_dst.clone();
            //self.stack.push(tmp_dst);
            //self.add_inst(Opcode::OpEq.to_operator())
            let mut x = self.stack.pop().unwrap();

            self.add_narrow_inst(&mut x, access_size);
            
            // Simple 'peek' ([n])
            if eq.is_empty() {
                continue;
            }

            // Simple 'poke' (=[n])
            if has_op.is_empty() {
                self.add_inst(Opcode::OpEq.to_operator());
                continue;
            }

            // 'poke' with another operation. (<op>=[n])
            let o = match self.opset.get(has_op) {
                Some(x) => x.clone(),
                // Return with error
                None => return, 
            };
            self.add_inst(o);
            // Reassignment.
            self.stack.push(tmp_dst1);
            self.add_inst(Opcode::OpEq.to_operator());
        }
    }

    pub fn emit_insts(&self) -> Vec<Instruction<'a>> {
        (self).insts.clone()
    }
}

#[test]
fn testing() {
	let mut p = Parser::new();
	p.parse("0,0x204db1,rip,+,[1],==,%z,zf,=,%b8,cf,=,%p,pf,=,%s,sf,=");
}
