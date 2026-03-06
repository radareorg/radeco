// Copyright (c) 2026, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

/// Represents error variants for the `analysis` module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AnalysisError {
    Domtree(String),
}

impl AnalysisError {
    /// Creates a new Domtree [AnalysisError].
    pub fn domtree<I: Into<String>>(err: I) -> Self {
        Self::Domtree(err.into())
    }
}

impl core::fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Domtree(err) => write!(f, "domtree: {err}"),
        }
    }
}

impl core::error::Error for AnalysisError {}
