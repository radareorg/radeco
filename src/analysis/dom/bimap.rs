// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

// Provides Bidirectional mapping for a pair values in an **O(1)** runtime and **O(n)** space
// elements mapped must be unique as the underlying type is a hashmap and to simplify
// synchronization between the hashmaps
// Using HashMaps, Note: Thread safety isn't guaranteed

// TODO: I guess the optimal solution is using 2 hash sets and a reference to link them
// to each other

use std::cmp::Eq;
use std::collections::HashMap;
use std::collections::hash_map::Iter;
use std::hash::Hash;
use std::iter::Iterator;

#[derive(Clone,Debug)]
pub struct BiMap<T, U>
    where T: Eq + Hash + Copy + Clone,
          U: Eq + Hash + Copy + Clone
{
    first_to_second: HashMap<T, U>,
    second_to_first: HashMap<U, T>,
}

impl<T, U> BiMap<T, U>
    where T: Eq + Hash + Copy + Clone,
          U: Eq + Hash + Copy + Clone
{
    // TODO: Add locking
    // TODO: Use 2 hashsets to save memory

    pub fn new() -> BiMap<T, U> {
        BiMap {
            first_to_second: HashMap::new(),
            second_to_first: HashMap::new(),
        }
    }

    pub fn insert(&mut self, first: T, second: U) {
        self.first_to_second.insert(first, second);
        self.second_to_first.insert(second, first);
    }

    pub fn remove(&mut self, first: T, second: U) {
        self.first_to_second.remove(&first);
        self.second_to_first.remove(&second);
    }

    pub fn first_from_second(&self, second: &U) -> Option<&T> {
        self.second_to_first.get(&second)
    }

    pub fn second_from_first(&self, first: &T) -> Option<&U> {
        self.first_to_second.get(first)
    }

    pub fn first_iter(&self) -> Iter<T, U> {
        self.first_to_second.iter()
    }

    pub fn second_iter(&self) -> Iter<U, T> {
        self.second_to_first.iter()
    }
}