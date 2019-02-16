use analysis::analyzer::{Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer};
use frontend::radeco_containers::RadecoFunction;
use analysis::matcher::gmatch;

use std::any::Any;

use petgraph::graph::NodeIndex;

#[derive(Debug)]
pub struct ArithChange {
    /// Index of the node to replace.
    pub old: NodeIndex,

    /// Old expression.
    pub old_expr: String,

    /// Replaced expression.
    pub new_expr: String,

    /// Bindings.
    pub bindings: Vec<(String, NodeIndex)>,
}

impl Change for ArithChange {
    fn as_any(&self) -> &dyn Any { self }
}

macro_rules! find_and_replace {
    ($ctx: expr, $f: expr => $r: expr) => {{
        let f = $f.to_string();
        let r = $r.to_string();

        let mut matcher = gmatch::GraphMatcher::new($ctx.0);
        let grep = matcher.grep(f.to_string());

        for m in grep {
            let action = $ctx.1(Box::new(ArithChange{
                old: m.get_root().clone(),
                old_expr: f.clone(),
                new_expr: r.clone(),
                bindings: m.get_bindings().clone()
            }));

            match action {
                Action::Apply => {
                    matcher.replace_value(m, r.clone());
                },
                Action::Skip => (),
                Action::Abort => return None,
            };
        }
    }}
}

const NAME: &str = "arithmetic";
const REQUIRES: &[AnalyzerKind] = &[];

pub const INFO: AnalyzerInfo = AnalyzerInfo {
    name: NAME,
    kind: AnalyzerKind::Arithmetic,
    requires: REQUIRES,
    uses_policy: true,
};

#[derive(Debug)]
pub struct Arithmetic { }

impl Arithmetic {
    pub fn new() -> Self {
        Arithmetic { }
    }
}

impl Analyzer for Arithmetic {
    fn info(&self) -> &'static AnalyzerInfo { &INFO }
    fn as_any(&self) -> &dyn Any { self }
}

impl FuncAnalyzer for Arithmetic {
    fn analyze<T: FnMut(Box<Change>) -> Action>(&mut self, func: &mut RadecoFunction, policy: Option<T>) -> Option<Box<AnalyzerResult>> {
        let policy = policy.expect("A policy function must be provided");
        let ssa = func.ssa_mut();
        let mut ctx = (ssa, policy);

        // Convert conditon codes into proper relational operators.
        find_and_replace!(ctx, "(OpNarrow1 (OpXor #x1, (OpSub %1, %2)))" => "(OpEq %1, %2)");
        find_and_replace!(ctx, "(OpNot (OpOr (OpEq %1, %2), (OpMov (OpLt %1, (OpSub %1, %2)))))" => "(OpGt %1, %2)");
        find_and_replace!(ctx, "(OpNot (OpMov (OpLt %1, (OpSub %1, %2))))"  => "(OpOr (OpGt %1, %2), (OpEq %1, %2))");
        find_and_replace!(ctx, "(OpMov (OpLt %1, (OpSub %1, %2)))" => "(OpLt %1, %2)");
        find_and_replace!(ctx, "(OpOr (OpEq %1, %2), (OpLt %1, %2))" => "(OpOr (OpLt %1, %2), (OpEq %1, %2))");

        // Some arithmetic identities.
        find_and_replace!(ctx, "(OpXor %1, %1)" => "(OpConst #x0)");
        find_and_replace!(ctx, "(OpMul %1, #x0)" => "(OpConst #x0");
        find_and_replace!(ctx, "(OpMul %1, #x1)" => "(OpConst %1");

        None
    }
}
