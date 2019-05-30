// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Components processing data in SSA form.

#[macro_use]
pub mod ssa {
    pub mod cfg_traits;
    pub mod graph_traits;
    #[macro_use]
    pub mod ssa_traits;
    pub mod error;
    pub mod memoryssa;
    pub mod ssadot;
    pub mod ssastorage;
    pub mod utils;
    #[allow(non_snake_case)]
    pub mod verifier;
}

#[macro_use]
pub mod dot;
pub mod ir;
pub mod ir_reader;
#[macro_use]
pub mod ir_writer;
#[allow(non_snake_case)]
pub mod phiplacement;
pub mod regfile;
