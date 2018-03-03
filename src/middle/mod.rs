// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Components processing data in SSA form.

pub mod dce;
pub mod dot;
pub mod ir;
pub mod ir_reader;
#[macro_use] pub mod ir_writer;
#[allow(non_snake_case)] pub mod phiplacement;
pub mod regfile;

#[macro_use]
pub mod ssa {
    pub mod graph_traits;
    pub mod cfg_traits;
    #[macro_use] pub mod ssa_traits;
    pub mod ssastorage;
    pub mod error;
    pub mod ssadot;
    pub mod memoryssa;
    #[allow(non_snake_case)] pub mod verifier;
}
