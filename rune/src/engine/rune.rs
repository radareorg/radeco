//! Trait and struct implementations for rune symbolic engine

use r2api::structs::LOpInfo;

use crate::context::context::{Context, RegisterRead};
use crate::explorer::explorer::PathExplorer;
use crate::stream::InstructionStream;
use crate::engine::engine::{Engine, EngineError, EngineResult};
use esil::lexer::{Token, Tokenizer};
use esil::parser::{Parse, Parser};

use libsmt::theories::{bitvec, core};
use libsmt::logics::qf_abv;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RuneControl {
    Continue,
    TerminatePath,
    ExploreTrue,
    ExploreFalse,
    Skip,
    Halt,
    Break,
}

pub struct Rune<Ctx, Exp, S>
    where Ctx: Context,
          Exp: PathExplorer,
          S: InstructionStream<Output = LOpInfo, Index = u64>
{
    /// Context on which the instance of rune operates on
    ctx: Ctx,
    /// Path decision algorithm
    explorer: Exp,
    /// Stores values that are intermediates during symbolic execution. These are not
    /// a part of register or memory
    intermediates: Vec<<Ctx as RegisterRead>::VarRef>,
    stream: S,
    skip: bool,
}


impl<S, Exp, Ctx> Rune<Ctx, Exp, S>
where Ctx: Context<IFn=qf_abv::QF_ABV_Fn>,
      Exp: PathExplorer<C = RuneControl, Ctx = Ctx>,
      S: InstructionStream<Output = LOpInfo, Index = u64>
{

    pub fn new(ctx: Ctx, exp: Exp, stream: S) -> Rune<Ctx, Exp, S> {
        Rune {
            ctx: ctx,
            explorer: exp,
            intermediates: Vec::new(),
            stream: stream,
            skip: false,
        }
    }

    fn process_in(&mut self,
                  t: Option<&Token>)
                  -> EngineResult<Option<<Ctx as RegisterRead>::VarRef>> {
        if t.is_none() {
            return Ok(None);
        }
        let read = match *t.unwrap() {
            Token::ERegister(ref name) | Token::EIdentifier(ref name) => {
                if self.ctx.alias_of(name.clone()) == Some("PC".to_owned()) {
                    let ip = self.ctx.ip();
                    self.ctx.define_const(ip, 64)
                } else {
                    self.ctx.reg_read(name)
                }
            }
            Token::EEntry(ref id, Some(64)) => self.intermediates[*id].clone(),
            Token::EConstant(value) => self.ctx.define_const(value, 64),
            Token::EAddress => {
                let ip = self.ctx.ip();
                self.ctx.define_const(ip, 64)
            }
            Token::EOld => self.ctx.e_old(),
            Token::ECur => self.ctx.e_cur(),
            Token::ELastsz => self.ctx.define_const(64, 64),
            _ => unreachable!(),
        };
        Ok(Some(read))
    }

    fn process_op(&mut self,
                  token: Token,
                  lhs: Option<Token>,
                  rhs: Option<Token>,
                  control: &mut RuneControl)
                  -> EngineResult<Option<<Ctx as RegisterRead>::VarRef>> {
        // Reset previously set `control`
        *control = RuneControl::Continue;

        // If skip is active, do no further processing.
        if self.skip {
            if token == Token::EEndIf {
                self.skip = false;
            }
            return Ok(None);
        }

        // asserts to check validity.
        if token.is_arity_zero() {
            return Ok(None);
        }
        // println!("****");

        // println!("OPERANDS TO {:?}: {:?} {:?}", token, lhs, rhs);
        let l_op = self.process_in(lhs.as_ref()).expect("LHS is ERR");
        let r_op = self.process_in(rhs.as_ref()).expect("RHS is ERR");
        // Since the operator arity us _atleast_ one. assert! that lhs is some.
        assert!(l_op.is_some());
        if token.is_binary() {
            assert!(r_op.is_some());
        }

        // Instructions that do not produce a result
        // Example: Mem Write / Eq / If / EndIf
        match token {
            Token::EEq => {
                let res = if let Some(Token::EIdentifier(ref reg)) = lhs {
                    if self.ctx.alias_of(reg.clone()) == Some("PC".to_owned()) {
                        if let Token::EConstant(const_) = rhs.unwrap() {
                            self.ctx.set_ip(const_);
                        }
                    } else {
                        // println!("REGISTER WRITE: {:?} = {:?}", reg, r_op);
                        self.ctx.reg_write(reg, r_op.unwrap());
                    }
                    Ok(None)
                } else {
                    Err(EngineError::InCorrectOperand)
                };
                return res;
            }
            Token::EIf => {
                *control = self.explorer.register_branch(&mut self.ctx, l_op.unwrap());
                return Ok(None);
            }
            Token::EPoke(size) => {
                self.ctx.mem_write(l_op.unwrap(), r_op.unwrap(), size as usize);
                return Ok(None)
            }
            Token::ENop => return Ok(None),
            _ => {}
        }

        let result = match token {
            Token::EPeek(size) => {
                self.ctx.mem_read(l_op.unwrap(), size as usize)
            }
            Token::ECmp | Token::ELt | Token::EGt => {
                // This case is a bit different as we want the result to be a bitvector rather
                // than a bool. Hence we adopt the following stratergy:
                // (ite (= lhs rhs) (_ bv1 64) (_ bv0 64))
                let e_cur = self.ctx.eval(bitvec::OpCodes::BvSub,
                                          vec![l_op.as_ref().unwrap().clone(),
                                               r_op.as_ref().unwrap().clone()]);
                self.ctx.set_e_cur(e_cur);
                self.ctx.set_e_old(l_op.as_ref().unwrap().clone());
                let const_0 = self.ctx.define_const(0, 64);
                let const_1 = self.ctx.define_const(1, 64);
                let eq = self.ctx.eval(token.to_smt(), vec![l_op.unwrap(), r_op.unwrap()]);
                self.ctx.eval(core::OpCodes::ITE, vec![eq, const_1, const_0])
            }
            Token::EPop => unimplemented!(),
            Token::EGoto => unimplemented!(),
            Token::EBreak => unimplemented!(),
            _ => {
                let operands = {
                    if token.is_unary() {
                        vec![l_op.unwrap()]
                    } else {
                        vec![l_op.unwrap(), r_op.unwrap()]
                    }
                };
                self.ctx.eval(token.to_smt(), operands)
            }
        };

        Ok(Some(result))
    }

    // Write out to intermediates and return a token to it.
    fn process_out(&mut self, res: &<Ctx as RegisterRead>::VarRef) -> Token {
        self.intermediates.push(res.clone());
        Token::EEntry(self.intermediates.len() - 1, Some(64))
    }
}

impl<Ctx, Exp, S> Engine for Rune<Ctx, Exp, S>
where Ctx: Context<IFn=qf_abv::QF_ABV_Fn>,
      Exp: PathExplorer<C = RuneControl, Ctx = Ctx>,
      S: InstructionStream<Output = LOpInfo, Index = u64>
{
    fn run(&mut self) -> EngineResult<()> {
        let mut p = Parser::init(None, Some(64));
        let mut control = RuneControl::Continue;

        loop {
            // println!("{}", self.ctx.ip());
            let opinfo = if let Some(opinfo_) = self.stream.at(self.ctx.ip()) {
                opinfo_
            } else if self.explorer.next_job(&mut self.ctx).is_some() {
                // Request for next instruction from queue
                self.stream.at(self.ctx.ip()).unwrap()
            } else {
                break;
            };

            let esil = opinfo.esil.as_ref().unwrap();

            // Increment ip by instruction width
            let width = opinfo.size.as_ref().unwrap();
            self.ctx.increment_ip(*width);

            loop {
                let token_opt = match p.parse::<_, Tokenizer>(esil) {
                    Ok(token_opt_) => token_opt_,
                    Err(_err) => {
                        panic!("ERROR tokenizing!");
                        continue;
                    }
                };
                if let Some(ref token) = token_opt {
                    // println!("{:?}", token);
                    // If skip is active, we do not want to modify the esil stack
                    let (lhs, rhs) = if self.skip {
                        (None, None)
                    } else {
                        match p.fetch_operands(token) {
                            Ok(operands_opt) => operands_opt,
                            Err(_err) => {
                                panic!("Error fetching operands!");
                                continue;
                            }
                        }
                    };

                    if let Ok(Some(ref res)) = self.process_op(token.clone(), lhs, rhs, &mut control) {
                        let rt = self.process_out(res);
                        p.push(rt);
                    }

                    // `ExploreTrue` -> Don't skip the section inside the ?{,...,}
                    // `ExploreFalse` -> Skip the section inside the ?{,...,}
                    match control {
                        RuneControl::ExploreTrue => {
                            self.skip = false;
                        }
                        RuneControl::ExploreFalse => {
                            self.skip = true;
                        }
                        RuneControl::Continue => continue,
                        _ => break,
                    }
                }
            }

            match self.explorer.next(&mut self.ctx) {
                RuneControl::Continue => {}
                _ => unimplemented!(),
            }

        }

        Ok(())
    }
}

trait ToSMTFn {
    fn to_smt(&self) -> qf_abv::QF_ABV_Fn;
}

// Implement Into<Qf_Abv_Fn> for tokens in order to use them with RuneContext.
impl ToSMTFn for Token {
    fn to_smt(&self) -> qf_abv::QF_ABV_Fn {
        match *self {
            Token::ECmp => core::OpCodes::Cmp.into(),
            Token::ELt => bitvec::OpCodes::BvULt.into(),
            Token::EGt => bitvec::OpCodes::BvUGt.into(),
            Token::ELsl => bitvec::OpCodes::BvShl.into(),
            Token::ELsr => bitvec::OpCodes::BvLShr.into(),
            Token::EAnd => bitvec::OpCodes::BvAnd.into(),
            Token::EOr => bitvec::OpCodes::BvOr.into(),
            Token::ENeg => bitvec::OpCodes::BvNeg.into(),
            Token::EMul => bitvec::OpCodes::BvMul.into(),
            Token::EXor => bitvec::OpCodes::BvXor.into(),
            Token::EAdd => bitvec::OpCodes::BvAdd.into(),
            Token::ESub => bitvec::OpCodes::BvSub.into(),
            Token::EDiv => bitvec::OpCodes::BvUDiv.into(),
            Token::EMod => bitvec::OpCodes::BvURem.into(),
            Token::ERor => unimplemented!(),
            Token::ERol => unimplemented!(),
            _ => panic!("This opcode is either unimplemented or is not an opcode at all!"),
        }
    }
}
