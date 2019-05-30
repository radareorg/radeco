// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use std::fmt::Debug;
use std::collections::VecDeque;

const ESIL_INTERNAL_PREFIX: char = '$';
const DEFAULT_SIZE: u8 = 64;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Esil Opcodes
    EInterrupt,
    ECmp,
    ELt,
    EGt,
    EEq,
    EIf,
    EEndIf,
    ELsl,
    ELsr,
    ERor,
    ERol,
    EAnd,
    EOr,
    ENop,
    ENeg,
    EMul,
    EXor,
    EAdd,
    ESub,
    EDiv,
    EMod,
    EPoke(u8),
    EPeek(u8),
    EDump,
    EPop,
    ETodo,
    EGoto,
    EBreak,
    EClear,
    EDup,
    ETrap,
    // Invalid
    EInvalid,
    // Parser Instructions.
    PCopy(usize),
    PPop(usize),
    PSync,
    // Esil Internal Vars
    IZero(u8),
    ICarry(u8),
    IParity(u8),
    IOverflow(u8),
    ISign(u8),
    IBorrow(u8),
    ISize(u8),
    IAddress(u8),
    IConstant(u64),
    // Esil Operands
    EConstant(u64),
    EIdentifier(String),
    // Custom type to allow pushing symbol table entries.
    // (entry_id, size - optional. Used to set lastsz.)
    // If no size is specified for the entry, then the default size from the parser is used to set
    // the lastsz.
    EEntry(usize, Option<u64>),
    ERegister(String),
    // Meta-variables
    // These are not emmitted by the lexer, but is used by the parser to communicate special
    // variables to the `Evaluator`.
    EOld,
    EOld_,
    ECur,
    ELastsz,
    EAddress,
}

impl Token {
    pub fn is_binary(&self) -> bool {
        match *self {
            Token::ECmp |
            Token::ELt |
            Token::EGt |
            Token::EEq |
            Token::ELsl |
            Token::ELsr |
            Token::ERor |
            Token::ERol |
            Token::EAnd |
            Token::EOr |
            Token::EMul |
            Token::EXor |
            Token::EAdd |
            Token::ESub |
            Token::EDiv |
            Token::EMod |
            Token::EPoke(_) => true,
            _ => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        match *self {
            Token::EPop | Token::ENeg | Token::EIf | Token::EPeek(_) => true,
            _ => false,
        }
    }

    pub fn is_arity_zero(&self) -> bool {
        match *self {
            Token::EDump | Token::ENop | Token::EEndIf => true,
            _ => false,
        }
    }

    pub fn is_implemented(&self) -> bool {
        match *self {
            Token::ETodo |
            Token::EInterrupt |
            Token::EGoto |
            Token::EBreak |
            Token::EClear |
            Token::ETrap => false,
            _ => true,
        }
    }

    pub fn is_meta(&self) -> bool {
        match *self {
            Token::EOld |
            Token::EOld_ |
            Token::ECur |
            Token::ELastsz |
            Token::EAddress => true,
            _ => false,
        }
    }

    pub fn should_set_vars(&self) -> bool {
        match *self {
            Token::ECmp | Token::EEq | Token::EPoke(_) | Token::EGt | Token::ELt => true,
            _ => false,
        }
    }

    pub fn updates_result(&self) -> bool {
        // If it an operator
        if self.is_binary() || self.is_unary() || self.is_arity_zero() {
            match *self {
                Token::EEq | Token::EPoke(_) => false,
                _ => true,
            }
        } else {
            false
        }
    }
}

pub trait Tokenize {
    type Token: Clone + Debug + PartialEq;
    fn tokenize<T: AsRef<str>>(esil: T) -> VecDeque<Self::Token>;
}

pub struct Tokenizer;

impl Tokenize for Tokenizer {
    type Token = Token;
    fn tokenize<T: AsRef<str>>(esil: T) -> VecDeque<Self::Token> {
        let mut tokens = VecDeque::new();
        for t in esil.as_ref().split(",").into_iter() {
            tokens.extend(
                match t {
                    "$" => vec![Token::EInterrupt],
                    "==" => vec![Token::ECmp],

                    "<" => vec![Token::ELt],
                    ">" => vec![Token::EGt],
                    "<=" => vec![Token::PCopy(2), Token::ELt, Token::PPop(2),
                    Token::ECmp, Token::EOr],
                    ">=" => vec![Token::PCopy(2), Token::EGt, Token::PPop(2),
                    Token::ECmp, Token::EOr],

                    "?{" => vec![Token::EIf],

                    "<<" => vec![Token::ELsl],
                    "<<=" => vec![Token::PCopy(1), Token::ELsl, Token::PPop(1),
                    Token::EEq],

                    ">>" => vec![Token::ELsr],
                    ">>=" => vec![Token::PCopy(1), Token::ELsr, Token::PPop(1),
                    Token::EEq],

                    ">>>" => vec![Token::ERor],
                    "<<<" => vec![Token::ERol],

                    "&" => vec![Token::EAnd],
                    "&=" => vec![Token::PCopy(1), Token::EAnd, Token::PPop(1),
                    Token::EEq],

                    "}" => vec![Token::EEndIf],

                    "|" => vec![Token::EOr],
                    "|=" => vec![Token::PCopy(1), Token::EOr, Token::PPop(1),
                    Token::EEq],

                    "!" => vec![Token::ENeg],
                    "!=" => vec![Token::PCopy(1), Token::ENeg, Token::PPop(1), Token::EEq],

                    "=" => vec![Token::EEq],

                    "*" => vec![Token::EMul],
                    "*=" => vec![Token::PCopy(1), Token::EMul, Token::PPop(1),
                    Token::EEq],

                    "^" => vec![Token::EXor],
                    "^=" => vec![Token::PCopy(1), Token::EXor, Token::PPop(1),
                    Token::EEq],

                    "+" => vec![Token::EAdd],
                    "+=" => vec![Token::PCopy(1), Token::EAdd, Token::PPop(1),
                    Token::EEq],

                    "++" => vec![Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1),
					Token::EAdd],
                    "++=" => vec![Token::PCopy(1), Token::PCopy(1), Token::EPop,
					Token::EConstant(1), Token::PPop(1), Token::EAdd, Token::PPop(1), Token::EEq],

                    "-" => vec![Token::ESub],
                    "-=" => vec![Token::PCopy(1), Token::ESub, Token::PPop(1),
                    Token::EEq],

                    "--" => vec![Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1),
					Token::ESub],
                    "--=" => vec![Token::PCopy(1), Token::PCopy(1), Token::EPop,
					Token::EConstant(1), Token::PPop(1), Token::ESub, Token::PPop(1), Token::EEq],

                    "/" => vec![Token::EDiv],
                    "/=" => vec![Token::PCopy(1), Token::EDiv, Token::PPop(1),
                    Token::EEq],

                    "%" => vec![Token::EMod],
                    "%=" => vec![Token::PCopy(1), Token::EMod, Token::PPop(1),
                    Token::EEq],

                    "=[]" => vec![Token::EPoke(64)],
                    "=[1]" => vec![Token::EPoke(8)],
                    "=[2]" => vec![Token::EPoke(16)],
                    "=[4]" => vec![Token::EPoke(32)],
                    "=[8]" => vec![Token::EPoke(64)],

                    "|=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EOr,
                    Token::PPop(1), Token::EPoke(64)],
                    "|=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::EOr,
                    Token::PPop(1), Token::EPoke(8)],
                    "|=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::EOr,
                    Token::PPop(1), Token::EPoke(16)],
                    "|=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::EOr,
                    Token::PPop(1), Token::EPoke(32)],
                    "|=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EOr,
                    Token::PPop(1), Token::EPoke(64)],

                    "^=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EXor,
                    Token::PPop(1), Token::EPoke(64)],
                    "^=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::EXor,
                    Token::PPop(1), Token::EPoke(8)],
                    "^=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::EXor,
                    Token::PPop(1), Token::EPoke(16)],
                    "^=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::EXor,
                    Token::PPop(1), Token::EPoke(32)],
                    "^=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EXor,
                    Token::PPop(1), Token::EPoke(64)],

                    "&=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EAnd,
                    Token::PPop(1), Token::EPoke(64)],
                    "&=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::EAnd,
                    Token::PPop(1), Token::EPoke(8)],
                    "&=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::EAnd,
                    Token::PPop(1), Token::EPoke(16)],
                    "&=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::EAnd,
                    Token::PPop(1), Token::EPoke(32)],
                    "&=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EAnd,
                    Token::PPop(1), Token::EPoke(64)],

                    "+=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EAdd,
                    Token::PPop(1), Token::EPoke(64)],
                    "+=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::EAdd,
                    Token::PPop(1), Token::EPoke(8)],
                    "+=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::EAdd,
                    Token::PPop(1), Token::EPoke(16)],
                    "+=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::EAdd,
                    Token::PPop(1), Token::EPoke(32)],
                    "+=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EAdd,
                    Token::PPop(1), Token::EPoke(64)],

                    "-=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::ESub,
                    Token::PPop(1), Token::EPoke(64)],
                    "-=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::ESub,
                    Token::PPop(1), Token::EPoke(8)],
                    "-=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::ESub,
                    Token::PPop(1), Token::EPoke(16)],
                    "-=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::ESub,
                    Token::PPop(1), Token::EPoke(32)],
                    "-=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::ESub,
                    Token::PPop(1), Token::EPoke(64)],

                    "%=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EMod,
                    Token::PPop(1), Token::EPoke(64)],
                    "%=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::EMod,
                    Token::PPop(1), Token::EPoke(8)],
                    "%=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::EMod,
                    Token::PPop(1), Token::EPoke(16)],
                    "%=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::EMod,
                    Token::PPop(1), Token::EPoke(32)],
                    "%=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EMod,
                    Token::PPop(1), Token::EPoke(64)],

                    "/=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EDiv,
                    Token::PPop(1), Token::EPoke(64)],
                    "/=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::EDiv,
                    Token::PPop(1), Token::EPoke(8)],
                    "/=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::EDiv,
                    Token::PPop(1), Token::EPoke(16)],
                    "/=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::EDiv,
                    Token::PPop(1), Token::EPoke(32)],
                    "/=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EDiv,
                    Token::PPop(1), Token::EPoke(64)],

                    "*=[]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EMul,
                    Token::PPop(1), Token::EPoke(64)],
                    "*=[1]" => vec![Token::PCopy(1), Token::EPeek(8), Token::EMul,
                    Token::PPop(1), Token::EPoke(8)],
                    "*=[2]" => vec![Token::PCopy(1), Token::EPeek(16), Token::EMul,
                    Token::PPop(1), Token::EPoke(16)],
                    "*=[4]" => vec![Token::PCopy(1), Token::EPeek(32), Token::EMul,
                    Token::PPop(1), Token::EPoke(32)],
                    "*=[8]" => vec![Token::PCopy(1), Token::EPeek(64), Token::EMul,
                    Token::PPop(1), Token::EPoke(64)],

                    "++=[]" => vec![Token::PCopy(1), Token::EPeek(64),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::EAdd,
                    Token::PPop(1), Token::EPoke(64)],
                    "++=[1]" => vec![Token::PCopy(1), Token::EPeek(8),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::EAdd,
                    Token::PPop(1), Token::EPoke(8)],
                    "++=[2]" => vec![Token::PCopy(1), Token::EPeek(16),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::EAdd,
                    Token::PPop(1), Token::EPoke(16)],
                    "++=[4]" => vec![Token::PCopy(1), Token::EPeek(32),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::EAdd,
                    Token::PPop(1), Token::EPoke(32)],
                    "++=[8]" => vec![Token::PCopy(1), Token::EPeek(64),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::EAdd,
                    Token::PPop(1), Token::EPoke(64)],

                    "--=[]" => vec![Token::PCopy(1), Token::EPeek(64),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::ESub,
                    Token::PPop(1), Token::EPoke(64)],
                    "--=[1]" => vec![Token::PCopy(1), Token::EPeek(8),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::ESub,
                    Token::PPop(1), Token::EPoke(8)],
                    "--=[2]" => vec![Token::PCopy(1), Token::EPeek(16),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::ESub,
                    Token::PPop(1), Token::EPoke(16)],
                    "--=[4]" => vec![Token::PCopy(1), Token::EPeek(32),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::ESub,
                    Token::PPop(1), Token::EPoke(32)],
                    "--=[8]" => vec![Token::PCopy(1), Token::EPeek(64),
                    Token::PCopy(1), Token::EPop, Token::EConstant(1), Token::PPop(1), Token::ESub,
                    Token::PPop(1), Token::EPoke(64)],

                    "[]" => vec![Token::EPeek(64)],
                    "[*]" => vec![Token::EPeek(64)],
                    "=[*]" => vec![Token::EPoke(64)],
                    "[1]" => vec![Token::EPeek(8)],
                    "[2]" => vec![Token::EPeek(16)],
                    "[4]" => vec![Token::EPeek(32)],
                    "[8]" => vec![Token::EPeek(64)],

                    "STACK" => vec![Token::EDump],
                    "POP" => vec![Token::EPop],
                    "TODO" => vec![Token::ETodo],
                    "GOTO" => vec![Token::EGoto],
                    "BREAK" => vec![Token::EBreak],
                    "CLEAR" => vec![Token::EClear],
                    "DUP" => vec![Token::EDup],
                    "TRAP" => vec![Token::ETrap],
                    _   => {
            // Handle internal vars
                        if Some(ESIL_INTERNAL_PREFIX) == t.chars().nth(0) {
                            let bit = if t.len() < 3 || !t.is_char_boundary(2) {
                                DEFAULT_SIZE
                            } else {
                                t[2..].parse::<u8>().unwrap_or(DEFAULT_SIZE)
                            };
                            match t.chars().nth(1).unwrap_or('\0') {
                                '$' => vec![Token::IAddress(bit)],
                                'z' => vec![Token::IZero(bit)],
                                'b' => vec![Token::IBorrow(bit)],
                                'c' => vec![Token::ICarry(bit)],
                                'p' => vec![Token::IParity(bit)],
                                'r' => vec![Token::ISize(bit)],
                                'o' => vec![Token::IOverflow(bit)],
                                's' => vec![Token::ISign(bit)],
                                _ => {
                                    if let Ok(num) = t[1..].parse::<u64>() {
                                        vec![Token::IConstant(num)]
                                    } else {
                                        vec![Token::EInvalid]
                                    }
                                }
                            }
                        } else if t.starts_with("0x") {
                            match u64::from_str_radix(t.trim_start_matches("0x"), 16) {
                                Ok(v) => vec![Token::EConstant(v)],
                                Err(_) => vec![Token::EInvalid],
                            }
                        } else if let Ok(v) = t.parse::<i64>() {
                            vec![Token::EConstant(v as u64)]
                        } else if let Ok(v) = t.parse::<u64>() {
                            vec![Token::EConstant(v)]
                        } else {
                            // Just returns it as an identifier. It is upto the
                            // parser to decide if it is a valid token.
                            vec![Token::EIdentifier(t.to_owned())]
                        }
                    }
                });
        }
        tokens
    }
}

#[cfg(test)]
mod test {
    use std::str;
    use super::*;

    #[test]
    fn esil_basic() {
        let op = vec![Token::EAdd];
        assert_eq!(op[0], Tokenizer::tokenize("+")[0]);
    }

    #[test]
    fn negative_int() {
        assert_eq!(Token::EConstant(0xFFFFFFFFFFFFFFFF),
                   Tokenizer::tokenize("-1")[0]);
    }

    #[test]
    fn u64_max_int() {
        assert_eq!(Token::EConstant(u64::max_value()),
                   Tokenizer::tokenize("18446744073709551615")[0]);
    }

    #[test]
    fn u64_min_int() {
        assert_eq!(Token::EConstant(0),
                   Tokenizer::tokenize(format!("{}", u64::min_value()))[0]);
    }

    #[test]
    fn i64_min_int() {
        assert_eq!(Token::EConstant(i64::min_value() as u64),
                   Tokenizer::tokenize(format!("{}", i64::min_value()))[0]);
    }

    #[test]
    fn i64_max_int() {
        assert_eq!(Token::EConstant(i64::max_value() as u64),
                   Tokenizer::tokenize(format!("{}", i64::max_value()))[0]);
    }

    #[test]
    fn utf8_internal_prefix() {
        Tokenizer::tokenize(str::from_utf8(&vec![0x24,0xda,0x91]).unwrap());
    }
}
