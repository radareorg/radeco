//! Trait and struct implementations for rune symbolic engine

use r2api::structs::LOpInfo;

use crate::context::{Context, RegisterRead};
use crate::explorer::PathExplorer;
use crate::stream::InstructionStream;

use super::{Engine, EngineError, EngineResult};

use esil::lexer::{Token, Tokenizer};
use esil::parser::{Parse, Parser};

use libsmt::logics::qf_abv;
use libsmt::theories::{bitvec, core};

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
where
    Ctx: Context,
    Exp: PathExplorer,
    S: InstructionStream<Output = LOpInfo, Index = u64>,
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
where
    Ctx: Context<IFn = qf_abv::QF_ABV_Fn>,
    Exp: PathExplorer<C = RuneControl, Ctx = Ctx>,
    S: InstructionStream<Output = LOpInfo, Index = u64>,
{
    pub fn new(ctx: Ctx, exp: Exp, stream: S) -> Rune<Ctx, Exp, S> {
        Rune {
            ctx,
            explorer: exp,
            intermediates: Vec::new(),
            stream,
            skip: false,
        }
    }

    fn process_in(
        &mut self,
        t: Option<&Token>,
    ) -> EngineResult<Option<<Ctx as RegisterRead>::VarRef>> {
        let Some(t) = t else {
            return Ok(None);
        };
        let read = match t {
            Token::ERegister(ref name) | Token::EIdentifier(ref name) => {
                if self.ctx.alias_of(name.clone()) == Some("PC".to_owned()) {
                    let ip = self.ctx.ip();
                    self.ctx.define_const(ip, 64)
                } else {
                    self.ctx.reg_read(name)?
                }
            }
            Token::EEntry(ref id, Some(64)) => self.intermediates[*id].clone(),
            Token::EConstant(value) => self.ctx.define_const(*value, 64),
            Token::EAddress => {
                let ip = self.ctx.ip();
                self.ctx.define_const(ip, 64)
            }
            Token::EOld => self.ctx.e_old()?,
            Token::ECur => self.ctx.e_cur()?,
            Token::ELastsz => self.ctx.define_const(64, 64),
            _ => return Err(EngineError::Unreachable),
        };
        Ok(Some(read))
    }

    fn process_op(
        &mut self,
        token: Token,
        lhs: Option<Token>,
        rhs: Option<Token>,
        control: &mut RuneControl,
    ) -> EngineResult<Option<<Ctx as RegisterRead>::VarRef>> {
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
        let l_op = self
            .process_in(lhs.as_ref())
            .and_then(|op| op.ok_or(EngineError::MissingLhs))?;
        let r_op = self.process_in(rhs.as_ref())?;
        // Since the operator arity us _atleast_ one. assert! that lhs is some.
        if token.is_binary() && r_op.is_none() {
            return Err(EngineError::MissingRhs);
        }

        // Instructions that do not produce a result
        // Example: Mem Write / Eq / If / EndIf
        match token {
            Token::EEq => {
                let res = if let Some(Token::EIdentifier(ref reg)) = lhs {
                    if self.ctx.alias_of(reg.clone()) == Some("PC".to_owned()) {
                        if let Token::EConstant(const_) = rhs.ok_or(EngineError::MissingRhs)? {
                            self.ctx.set_ip(const_);
                        }
                    } else {
                        // println!("REGISTER WRITE: {:?} = {:?}", reg, r_op);
                        r_op.ok_or(EngineError::MissingRhs).and_then(|op| {
                            self.ctx.reg_write(reg, op).map_err(EngineError::from)
                        })?;
                    }
                    Ok(None)
                } else {
                    Err(EngineError::IncorrectOperand)
                };
                return res;
            }
            Token::EIf => {
                *control = self.explorer.register_branch(&mut self.ctx, l_op)?;
                return Ok(None);
            }
            Token::EPoke(size) => {
                self.ctx
                    .mem_write(l_op, r_op.ok_or(EngineError::MissingRhs)?, size as usize)?;
                return Ok(None);
            }
            Token::ENop => return Ok(None),
            _ => {}
        }

        let result = match token {
            Token::EPeek(size) => self.ctx.mem_read(l_op, size as usize)?,
            Token::ECmp | Token::ELt | Token::EGt => {
                // This case is a bit different as we want the result to be a bitvector rather
                // than a bool. Hence we adopt the following stratergy:
                // (ite (= lhs rhs) (_ bv1 64) (_ bv0 64))
                let e_cur = self.ctx.eval(
                    bitvec::OpCodes::BvSub,
                    vec![l_op.clone(), r_op.clone().ok_or(EngineError::MissingRhs)?],
                );
                self.ctx.set_e_cur(e_cur);
                self.ctx.set_e_old(l_op.clone());
                let const_0 = self.ctx.define_const(0, 64);
                let const_1 = self.ctx.define_const(1, 64);
                let eq = self.ctx.eval(
                    token.to_smt()?,
                    vec![l_op.clone(), r_op.ok_or(EngineError::MissingRhs)?],
                );
                self.ctx
                    .eval(core::OpCodes::ITE, vec![eq, const_1, const_0])
            }
            Token::EPop => return Err(EngineError::Unimplemented),
            Token::EGoto => return Err(EngineError::Unimplemented),
            Token::EBreak => return Err(EngineError::Unimplemented),
            _ => {
                let operands = if token.is_unary() {
                    vec![l_op]
                } else {
                    vec![l_op, r_op.ok_or(EngineError::MissingRhs)?]
                };
                token.to_smt().map(|smt| self.ctx.eval(smt, operands))?
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
where
    Ctx: Context<IFn = qf_abv::QF_ABV_Fn>,
    Exp: PathExplorer<C = RuneControl, Ctx = Ctx>,
    S: InstructionStream<Output = LOpInfo, Index = u64>,
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
                self.stream
                    .at(self.ctx.ip())
                    .ok_or(EngineError::MissingNextInstr)?
            } else {
                break;
            };

            let esil = opinfo.esil.as_ref().ok_or(EngineError::MissingEsil)?;

            // Increment ip by instruction width
            let width = opinfo.size.as_ref().ok_or(EngineError::MissingOpInfoSize)?;
            self.ctx.increment_ip(*width);

            loop {
                let token_opt = p
                    .parse::<_, Tokenizer>(esil)
                    .map_err(|_| EngineError::Tokenizing)?;

                if let Some(ref token) = token_opt {
                    // println!("{:?}", token);
                    // If skip is active, we do not want to modify the esil stack
                    let (lhs, rhs) = if self.skip {
                        (None, None)
                    } else {
                        p.fetch_operands(token)
                            .map_err(|_| EngineError::FetchOperand)?
                    };

                    if let Ok(Some(ref res)) =
                        self.process_op(token.clone(), lhs, rhs, &mut control)
                    {
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

            if !matches!(self.explorer.next(&mut self.ctx), RuneControl::Continue) {
                return Err(EngineError::Unimplemented);
            }
        }

        Ok(())
    }
}

trait ToSMTFn {
    fn to_smt(&self) -> EngineResult<qf_abv::QF_ABV_Fn>;
}

// Implement Into<Qf_Abv_Fn> for tokens in order to use them with RuneContext.
impl ToSMTFn for Token {
    fn to_smt(&self) -> EngineResult<qf_abv::QF_ABV_Fn> {
        match *self {
            Token::ECmp => Ok(core::OpCodes::Cmp.into()),
            Token::ELt => Ok(bitvec::OpCodes::BvULt.into()),
            Token::EGt => Ok(bitvec::OpCodes::BvUGt.into()),
            Token::ELsl => Ok(bitvec::OpCodes::BvShl.into()),
            Token::ELsr => Ok(bitvec::OpCodes::BvLShr.into()),
            Token::EAnd => Ok(bitvec::OpCodes::BvAnd.into()),
            Token::EOr => Ok(bitvec::OpCodes::BvOr.into()),
            Token::ENeg => Ok(bitvec::OpCodes::BvNeg.into()),
            Token::EMul => Ok(bitvec::OpCodes::BvMul.into()),
            Token::EXor => Ok(bitvec::OpCodes::BvXor.into()),
            Token::EAdd => Ok(bitvec::OpCodes::BvAdd.into()),
            Token::ESub => Ok(bitvec::OpCodes::BvSub.into()),
            Token::EDiv => Ok(bitvec::OpCodes::BvUDiv.into()),
            Token::EMod => Ok(bitvec::OpCodes::BvURem.into()),
            Token::ERor => Err(EngineError::Unimplemented),
            Token::ERol => Err(EngineError::Unimplemented),
            _ => Err(EngineError::InvalidOpCode),
        }
    }
}
