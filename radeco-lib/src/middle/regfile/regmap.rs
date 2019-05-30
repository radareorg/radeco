use crate::middle::regfile::RegisterId;

use vec_map::{self, VecMap};

use std::ops;

/// A map with [`RegisterId`]s as keys.
#[derive(Debug, Clone)]
pub struct RegisterMap<V> {
    map: VecMap<V>,
}

impl<V> RegisterMap<V> {
    pub(super) fn with_register_count(regcount: usize) -> Self {
        RegisterMap {
            map: VecMap::with_capacity(regcount),
        }
    }
    pub fn get(&self, key: RegisterId) -> Option<&V> {
        self.map.get(key.to_usize())
    }
    pub fn get_mut(&mut self, key: RegisterId) -> Option<&mut V> {
        self.map.get_mut(key.to_usize())
    }
    pub fn insert(&mut self, key: RegisterId, value: V) -> Option<V> {
        assert!(
            key.to_usize() < self.map.capacity(),
            "invalid regid: {:?}",
            key
        );
        self.map.insert(key.to_usize(), value)
    }
    pub fn remove(&mut self, key: RegisterId) -> Option<V> {
        self.map.remove(key.to_usize())
    }
    pub fn iter(&self) -> Iter<V> {
        self.into_iter()
    }
    pub fn iter_mut(&mut self) -> IterMut<V> {
        self.into_iter()
    }
}

impl<V> ops::Index<RegisterId> for RegisterMap<V> {
    type Output = V;
    fn index(&self, i: RegisterId) -> &V {
        &self.map[i.to_usize()]
    }
}
impl<V> ops::IndexMut<RegisterId> for RegisterMap<V> {
    fn index_mut(&mut self, i: RegisterId) -> &mut V {
        &mut self.map[i.to_usize()]
    }
}
impl<V> Extend<(RegisterId, V)> for RegisterMap<V> {
    fn extend<I: IntoIterator<Item = (RegisterId, V)>>(&mut self, iter: I) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

macro_rules! delgate_iter {
    ($name:ident<$($g:tt),+>, $vty:ty) => {
        impl<$($g),+> Iterator for $name<$($g),+> {
            type Item = (RegisterId, $vty);
            fn next(&mut self) -> Option<Self::Item> {
                self.0.next().map(|(k, v)| (RegisterId::from_usize(k), v))
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.0.size_hint()
            }
        }
        impl<$($g),+> ExactSizeIterator for $name<$($g),+> {}
        impl<$($g),+> DoubleEndedIterator for $name<$($g),+> {
            fn next_back(&mut self) -> Option<Self::Item> {
                self.0
                    .next_back()
                    .map(|(k, v)| (RegisterId::from_usize(k), v))
            }
        }
    }
}
macro_rules! delgate_into_iter {
    ($name:ident<$($g:tt),+>, $for:ty, $vty:ty, $method:ident) => {
        impl<$($g),+> IntoIterator for $for {
            type Item = (RegisterId, $vty);
            type IntoIter = $name<$($g),+>;
            fn into_iter(self) -> Self::IntoIter {
                $name(self.map.$method())
            }
        }
        delgate_iter!($name<$($g),+>, $vty);
    }
}
pub struct IntoIter<V>(vec_map::IntoIter<V>);
delgate_into_iter!(IntoIter<V>, RegisterMap<V>, V, into_iter);
pub struct Iter<'a, V: 'a>(vec_map::Iter<'a, V>);
delgate_into_iter!(Iter<'a, V>, &'a RegisterMap<V>, &'a V, iter);
pub struct IterMut<'a, V: 'a>(vec_map::IterMut<'a, V>);
delgate_into_iter!(IterMut<'a, V>, &'a mut RegisterMap<V>, &'a mut V, iter_mut);
