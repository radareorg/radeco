// rust, y u no provide integer trait

use std::cmp::{Eq, Ord};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Add, Not};

pub trait IndexType : Clone + Copy + Add<Output=Self> + Not<Output=Self> + Eq + Ord + Debug + Hash {
	// replace with zero trait once stable
	fn zero() -> Self;
	fn as_usize(self) -> usize;
	fn from_usize(usize) -> Self;
	fn invalid() -> Self { !(Self::zero()) }
}

impl IndexType for i8 {
	fn zero() -> i8 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i8 { v as i8 }
}

impl IndexType for i16 {
	fn zero() -> i16 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i16 { v as i16 }
}

impl IndexType for i32 {
	fn zero() -> i32 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i32 { v as i32 }
}

impl IndexType for i64 {
	fn zero() -> i64 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i64 { v as i64 }
}

impl IndexType for isize {
	fn zero() -> isize { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> isize { v as isize }
}
