extern crate petgraph;
pub mod basicblock;
pub mod graph;
pub mod instruction;

// private

use std::ops::Add;

trait InnerIndexType : Clone + Copy + Add<Output=Self> {
	// replace with zero trait once stable
	fn zero() -> Self;
}
impl InnerIndexType for i8 { fn zero() -> i8 { 0 } }
impl InnerIndexType for i16 { fn zero() -> i16 { 0 } }
impl InnerIndexType for i32 { fn zero() -> i32 { 0 } }
impl InnerIndexType for i64 { fn zero() -> i64 { 0 } }
impl InnerIndexType for isize { fn zero() -> isize { 0 } }

pub trait KnowsIndexType { type I: InnerIndexType; }
