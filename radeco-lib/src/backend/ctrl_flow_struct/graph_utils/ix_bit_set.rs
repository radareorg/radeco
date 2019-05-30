//! see [`IxBitSet`]

use bit_set::{self, BitSet};
use std::iter::{Extend, FromIterator};
use std::marker::PhantomData;

pub trait IndexLike {
    fn new(x: usize) -> Self;
    fn index(self) -> usize;
}

macro_rules! forward {
    (pub fn $name:ident(&self) -> $out:ty) => (
        pub fn $name(&self) -> $out { (self.0).$name() }
    );
    (pub fn $name:ident(&mut self) -> $out:ty) => (
        pub fn $name(&mut self) -> $out { (self.0).$name() }
    );
    (pub fn $name:ident(&mut self, value: Ix) -> $out:ty) => (
        pub fn $name(&mut self, value: Ix) -> $out { (self.0).$name(value.index()) }
    );
    (pub fn $name:ident(&self, value: Ix) -> $out:ty) => (
        pub fn $name(&self, value: Ix) -> $out { (self.0).$name(value.index()) }
    );
    (pub fn $name:ident(&mut self, other: &Self) -> $out:ty) => (
        pub fn $name(&mut self, other: &Self) -> $out { (self.0).$name(&other.0) }
    );
}

/// Wrapper for [`BitSet`] for [`IndexLike`] things.
#[derive(Clone, Default, PartialOrd, Ord, PartialEq, Eq, Debug, Hash)]
pub struct IxBitSet<Ix: IndexLike>(BitSet, PhantomData<Ix>);

impl<Ix: IndexLike> IxBitSet<Ix> {
    pub fn new() -> Self {
        IxBitSet(BitSet::new(), PhantomData)
    }

    pub fn with_capacity(nbits: usize) -> Self {
        IxBitSet(BitSet::with_capacity(nbits), PhantomData)
    }

    pub fn iter(&self) -> Iter<Ix> {
        Iter(self.0.iter(), PhantomData)
    }

    forward!(pub fn capacity(&self) -> usize);
    forward!(pub fn len(&self) -> usize);
    forward!(pub fn is_empty(&self) -> bool);
    forward!(pub fn insert(&mut self, value: Ix) -> bool);
    forward!(pub fn remove(&mut self, value: Ix) -> bool);
    forward!(pub fn contains(&self, value: Ix) -> bool);
    forward!(pub fn clear(&mut self) -> ());
    forward!(pub fn union_with(&mut self, other: &Self) -> ());
    forward!(pub fn intersect_with(&mut self, other: &Self) -> ());
    forward!(pub fn difference_with(&mut self, other: &Self) -> ());
    forward!(pub fn symmetric_difference_with(&mut self, other: &Self) -> ());
}

impl<Ix: IndexLike> FromIterator<Ix> for IxBitSet<Ix> {
    fn from_iter<I: IntoIterator<Item = Ix>>(iter: I) -> Self {
        let mut ret = IxBitSet::new();
        ret.extend(iter);
        ret
    }
}

impl<Ix: IndexLike> Extend<Ix> for IxBitSet<Ix> {
    fn extend<I: IntoIterator<Item = Ix>>(&mut self, iter: I) -> () {
        for i in iter {
            self.insert(i);
        }
    }
}

impl<'a, Ix: IndexLike + Copy + 'a> FromIterator<&'a Ix> for IxBitSet<Ix> {
    fn from_iter<I: IntoIterator<Item = &'a Ix>>(iter: I) -> Self {
        let mut ret = IxBitSet::new();
        ret.extend(iter);
        ret
    }
}

impl<'a, Ix: IndexLike + Copy + 'a> Extend<&'a Ix> for IxBitSet<Ix> {
    fn extend<I: IntoIterator<Item = &'a Ix>>(&mut self, iter: I) -> () {
        for &i in iter {
            self.insert(i);
        }
    }
}

impl<'a, Ix: IndexLike> IntoIterator for &'a IxBitSet<Ix> {
    type Item = Ix;
    type IntoIter = Iter<'a, Ix>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Clone)]
pub struct Iter<'a, Ix: IndexLike>(bit_set::Iter<'a, u32>, PhantomData<Ix>);

impl<'a, Ix: IndexLike> Iterator for Iter<'a, Ix> {
    type Item = Ix;
    fn next(&mut self) -> Option<Ix> {
        self.0.next().map(|i| Ix::new(i))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
