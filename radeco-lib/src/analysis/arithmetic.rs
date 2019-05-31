use crate::analysis::analyzer::{
    Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer,
};
use crate::analysis::matcher::gmatch;
use crate::frontend::radeco_containers::RadecoFunction;

use std::any::Any;
use std::io;
use std::io::{BufRead, BufReader, ErrorKind};

use petgraph::graph::NodeIndex;

fn load_patterns() -> io::Result<Vec<(String, String)>> {
    let patterns_str = include_str!("../../analysis/patterns");
    let reader = BufReader::new(patterns_str.as_bytes());

    let mut identities = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let line = line.trim();

        if line.len() == 0 || line.starts_with('#') {
            continue;
        }

        let mut tokens = line.split("=>");

        let error = io::Error::new(ErrorKind::InvalidInput, "invalid pattern");
        let old = tokens.next().ok_or(error)?;

        let error = io::Error::new(ErrorKind::InvalidInput, "invalid pattern");
        let new = tokens.next().ok_or(error)?;

        identities.push((old.trim().to_string(), new.trim().to_string()));
    }

    Ok(identities)
}

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
    fn as_any(&self) -> &dyn Any {
        self
    }
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
pub struct Arithmetic {
    replace_patterns: Vec<(String, String)>,
}

impl Arithmetic {
    pub fn new() -> Self {
        let replace_patterns = load_patterns().unwrap_or_else(|_e| {
            radeco_err!("Failed to load the replace patterns: {}", _e);
            Vec::new()
        });

        Arithmetic {
            replace_patterns: replace_patterns,
        }
    }
}

impl Analyzer for Arithmetic {
    fn info(&self) -> &'static AnalyzerInfo {
        &INFO
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl FuncAnalyzer for Arithmetic {
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        func: &mut RadecoFunction,
        policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>> {
        let mut policy = policy.expect("A policy function must be provided");
        let ssa = func.ssa_mut();

        for (old, new) in self.replace_patterns.iter() {
            let mut matcher = gmatch::GraphMatcher::new(ssa);
            let grep = matcher.grep(old.to_string());

            for m in grep {
                let action = policy(Box::new(ArithChange {
                    old: m.get_root().clone(),
                    old_expr: old.clone(),
                    new_expr: new.clone(),
                    bindings: m.get_bindings().clone(),
                }));

                match action {
                    Action::Apply => {
                        matcher.replace_value(m, new.clone());
                    }
                    Action::Skip => (),
                    Action::Abort => return None,
                };
            }
        }

        None
    }
}
