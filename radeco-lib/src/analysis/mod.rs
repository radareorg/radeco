// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements analysis and optimizations on radeco IR.

pub mod analyzer;
pub mod engine;

#[allow(dead_code)]
// pub mod valueset;
// pub mod propagate;
pub mod dce;
pub mod dom;
pub mod sccp;
pub mod cse {
    pub mod cse;
    pub mod ssasort;
}

#[macro_use]
pub mod matcher {
    #[macro_use]
    pub mod gmatch;
}

pub mod arithmetic;
pub mod constraint_set;
pub mod copy_propagation;
pub mod functions;
pub mod inst_combine;
pub mod interproc;
pub mod mask2narrow;
pub mod reference_marking;
pub mod tie;
pub mod vsa;
