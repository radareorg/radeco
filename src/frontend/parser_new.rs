use esil::parser::Parse;
use esil::lexer::{Token, Tokenize, Tokenizer};
use std::io::BufRead;
use std::marker::PhantomData;

// Some design notes on the parser
// Based on the previous parser and the experience in building that, here are some points that we'd
// like to improve on:
//   - Don't use strings for temp names. Rather use integers are comparison is faster.
//   - Directly build an AST rather performing this transformation later on.
//   - Be able to accept ESIL from any source, not just r2. This is currently being implemented bu
//   using `R: BufRead`. Hence, any struct that implements BufRead can be used here. This allows us
//   to write ESIL in files and load it to test without having to use r2.
//   - Try to build the CFG on the fly. This may not be possible or maybe costly. Further
//   investigation and discussion is required before we decide this.

pub struct Evaluator<T, L, R>
    where T: Parse,
          L: Tokenize<Token = T::InType>,
          R: BufRead
{
    parser: T,
    lexer: PhantomData<L>,
    source: Option<R>,
    
}

impl<T, L, R> Evaluator<T, L, R>
where T: Parse<InType = Token, OutType = Token>,
      L: Tokenize<Token = T::InType>,
      R: BufRead
{
    pub fn new(source: Option<R>, parser: T) -> Evaluator<T, L, R> {
        Evaluator {
            parser: parser,
            source: source,
            lexer: PhantomData,
        }
    }

    fn next_instruction(&mut self) -> Option<String> {
        self.source.as_mut().unwrap().lines().next().and_then(|r| r.ok())
    }

    pub fn run(&mut self) {
        while let Some(ref inst) = self.next_instruction() {
            while let Some(ref t) = self.parser.parse::<_, L>(inst) {
                let (lhs, rhs) = self.parser.fetch_operands(t);
                println!("{:?} {:?} {:?}", t, lhs, rhs);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use esil::parser::Parser;
    use esil::lexer::Tokenizer;

    #[test]
    fn parse_dummy() {
        use std::io::BufReader;
        use std::io::prelude::*;
        use std::fs::File;

        let f = File::open("foo.txt").unwrap();
        let f = BufReader::new(f);

        let mut p = Parser::init(None);

        let mut evaluator = Evaluator::<_, Tokenizer, _>::new(Some(f), p);
        evaluator.run();
    }
}
