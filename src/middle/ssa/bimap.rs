// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.
#![allow(dead_code)]
//! This module implements a simple bidirectional map that handles insert,
//! update and remove.
//!
//! ## panic
//! It is important to note that for a bidirectional map to work the mapping
//! should be one-to-one to ensure unique inverse. Hence, an attempt to violate
//! this will cause this module to panic.


use std::collections::HashMap;
use std::cmp::Eq;
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct BiMap<K: Hash + Eq + Clone, V: Hash + Eq + Clone> {
	f: HashMap<K, V>,
	b: HashMap<V, K>,
}

impl<K: Hash + Eq + Clone, V: Hash + Eq + Clone> BiMap<K, V> {
	pub fn new() -> BiMap<K, V> {
		BiMap {
			f: HashMap::new(),
			b: HashMap::new(),
		}
	}

	pub fn insert(&mut self, k: K, v: V) {
		if let Some(ref x) = self.f.insert(k.clone(), v.clone()) {
			self.b.remove(x);
		}
		if let Some(_) = self.b.insert(v, k) {
			panic!("Failed Assertion. BiMap is no longer one-to-one!");
		}
	}

	pub fn remove_k(&mut self, k: &K) -> Option<V> {
		match self.f.get(k) {
			None => { return None; },
			Some(v) => { self.b.remove(v); },
		}
		self.f.remove(k)
	}

	pub fn remove_v(&mut self, v: &V) -> Option<K> {
		match self.b.get(v) {
			None => { return None; },
			Some(k) => { self.f.remove(k); },
		}
		self.b.remove(v)
	}

	pub fn get(&self, k: &K) -> Option<&V> {
		self.f.get(k)
	}

	pub fn get_inverse(&self, k: &V) -> Option<&K> {
		self.b.get(k)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn bimap_insert() {
		let mut h = BiMap::<usize, usize>::new();
		h.insert(1, 5);
		assert_eq!(*(h.get(&1).unwrap()), 5);
		assert_eq!(*(h.get_inverse(&5).unwrap()), 1);
	}

	#[test]
	fn bimap_remove_k() {
		let mut h = BiMap::<usize, usize>::new();
		h.insert(1, 5);
		h.remove_k(&1);
		assert!(h.get(&1).is_none());
		assert!(h.get_inverse(&5).is_none());
	}

	#[test]
	fn bimap_remove_v() {
		let mut h = BiMap::<usize, usize>::new();
		h.insert(1, 5);
		h.remove_v(&5);
		assert!(h.get(&1).is_none());
		assert!(h.get_inverse(&5).is_none());
	}

	#[test]
	fn bimap_update() {
		let mut h = BiMap::<usize, usize>::new();
		h.insert(1, 5);
		h.insert(1, 6);

		assert_eq!(*(h.get(&1).unwrap()), 6);
		assert!(h.get_inverse(&5).is_none());
		assert_eq!(*(h.get_inverse(&6).unwrap()), 1);
	}

	#[test]
	#[should_panic]
	fn bimap_one_to_one() {
		let mut h = BiMap::<usize, usize>::new();
		h.insert(1, 5);
		h.insert(2, 6);
		h.insert(1, 6);
	}
}
