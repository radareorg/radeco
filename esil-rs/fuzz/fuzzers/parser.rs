#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate esil;

use std::collections::HashMap;
use std::str;

use esil::parser::{Parse, Parser};
use esil::lexer::{Token, Tokenizer};

fuzz_target!(|data: &[u8]| {
    if let Ok(esil) = str::from_utf8(data) {
        let regset: HashMap<String, u64>  = {
            let mut regset = HashMap::new();
            regset.insert("rax".to_owned(), 64);
            regset.insert("rbx".to_owned(), 64);
            regset.insert("rcx".to_owned(), 64);
            regset.insert("eax".to_owned(), 32);
            regset.insert("ebx".to_owned(), 32);
            regset.insert("ecx".to_owned(), 32);
            regset.insert("zf".to_owned(), 1);
            regset.insert("pf".to_owned(), 1);
            regset.insert("cf".to_owned(), 1);
            regset.insert("of".to_owned(), 1);
            regset.insert("sf".to_owned(), 1);
            regset
        };
        let mut parser = Parser::init(Some(regset), Some(64));
        parser.lastsz = Some(Token::EConstant(64));
        while let Ok(Some(ref token)) = parser.parse::<_, Tokenizer>(esil) {
            let _ = parser.fetch_operands(token);
        }
    }
});
