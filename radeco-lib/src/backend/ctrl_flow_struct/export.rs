use super::ast::{AstNode as AstNodeC, LoopType};
use super::condition;
use super::{AstNode, Condition};
use crate::backend::lang_c::c_ast::{self, CAST};
use crate::backend::lang_c::c_cfg::{CASTConverter, CASTRef, CCFGRef, CCFG};

pub fn to_c_ast<'cd>(ccfg: &CCFG, ast: AstNode<'cd, CCFG>) -> Result<CAST, &'static str> {
    let mut conv = CASTConverter::new(ccfg);
    conv.init();
    let consts = conv.ast_mut().declare_vars(
        c_ast::Ty::new(c_ast::BTy::Int, false, 0),
        &["0".to_owned(), "1".to_owned()],
        true,
    );
    let mut exporter = Exporter {
        conv,
        const_zero: consts[0],
        const_one: consts[1],
    };
    exporter.go(ast)?;
    Ok(exporter.conv.into_ast())
}

struct Exporter<'a> {
    conv: CASTConverter<'a>,
    const_zero: CASTRef,
    const_one: CASTRef,
}
impl<'a> Exporter<'a> {
    fn go<'cd>(&mut self, ast: AstNode<'cd, CCFG>) -> Result<Vec<CASTRef>, &'static str> {
        use self::AstNodeC::*;
        use self::LoopType::*;
        match ast {
            BasicBlock(b) => b
                .into_iter()
                .map(|c| self.conv.to_c_ast_single(c))
                .collect(),
            Seq(seq) => {
                let seq = seq
                    .into_iter()
                    .map(|a| self.go(a))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(seq.into_iter().flat_map(|v| v).collect())
            }
            Cond(c, t, oe) => {
                let c = c.fold(&mut *self)?;
                let t = self.go(*t)?;
                let oe = transpose(oe.map(|e| self.go(*e)))?;
                Ok(vec![self.conv.ast_mut().new_if(c, t, oe.map(|e| e))])
            }
            Loop(PreChecked(c), b) => {
                let c = c.fold(&mut *self)?;
                let b = self.go(*b)?;
                Ok(vec![self.conv.ast_mut().new_while(c, b)])
            }
            Loop(PostChecked(c), b) => {
                let c = c.fold(&mut *self)?;
                let b = self.go(*b)?;
                Ok(vec![self.conv.ast_mut().new_do_while(c, b)])
            }
            Loop(Endless, b) => {
                let c = self.const_one;
                let b = self.go(*b)?;
                Ok(vec![self.conv.ast_mut().new_while(c, b)])
            }
            Break => Ok(vec![self.conv.ast_mut().insert_break()]),
            Switch(_, _, _) => unimplemented!(), // TODO
        }
    }

    fn bool_expr<'c, I: IntoIterator<Item = Condition<'c, CCFG>>>(
        &mut self,
        empty: CASTRef,
        op: fn() -> c_ast::Expr,
        iter: I,
    ) -> Result<CASTRef, &'static str> {
        let mut iter = iter.into_iter();
        let opt_first = iter.next();
        if let Some(first) = opt_first {
            let mut cur = first.fold(&mut *self)?;
            for opn in iter {
                let opn = opn.fold(&mut *self)?;
                cur = self.conv.ast_mut().expr(op(), &[cur, opn], true);
            }
            Ok(cur)
        } else {
            Ok(empty)
        }
    }
}

impl<'a, 'b> condition::Folder<CCFGRef> for &'a mut Exporter<'b> {
    type Output = Result<CASTRef, &'static str>;
    fn var(&mut self, inverted: bool, &var: &CCFGRef) -> Self::Output {
        let v = self
            .conv
            .get_node(var)
            .ok_or("unconverted expression in condition")?;
        if inverted {
            Ok(self.conv.ast_mut().expr(c_ast::Expr::Not, &[v], true))
        } else {
            Ok(v)
        }
    }

    fn and<'c, I>(&mut self, operands: I) -> Self::Output
    where
        I: IntoIterator<Item = Condition<'c, CCFG>>,
    {
        let empty = self.const_zero;
        self.bool_expr(empty, || c_ast::Expr::And, operands)
    }

    fn or<'c, I>(&mut self, operands: I) -> Self::Output
    where
        I: IntoIterator<Item = Condition<'c, CCFG>>,
    {
        let empty = self.const_one;
        self.bool_expr(empty, || c_ast::Expr::Or, operands)
    }
}

fn transpose<T, E>(o: Option<Result<T, E>>) -> Result<Option<T>, E> {
    match o {
        Some(Ok(t)) => Ok(Some(t)),
        Some(Err(e)) => Err(e),
        None => Ok(None),
    }
}
