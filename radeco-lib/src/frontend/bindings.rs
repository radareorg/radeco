//! Defines structs and methods that describe a bindings.

use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::ops::{Index, IndexMut};
use std::process;

pub trait RBind {
    type SSARef: fmt::Debug + Clone;

    fn is_argument(&self) -> bool;
    fn is_local(&self) -> bool;
    fn is_modified(&self) -> bool;
    fn is_preserved(&self) -> bool;
    fn is_return(&self) -> bool;

    fn mark_argument(&mut self);
    fn mark_modified(&mut self);
    fn mark_preserved(&mut self);
    fn mark_return(&mut self);

    fn name(&self) -> String;
    fn set_name(&mut self, _: String);

    fn is_register(&self) -> bool;
    fn mark_register(&mut self, _: String);

    fn is_memory(&self) -> bool {
        !self.is_register() && !self.is_unknown()
    }
    fn mark_memory(&mut self);

    fn is_fn_local(&self) -> bool;
    fn mark_fn_local(&mut self, _: usize, _: i64);
    fn local_info(&self) -> LocalInfo;

    fn is_stack(&self) -> bool;
    fn mark_stack(&mut self);

    fn is_global(&self) -> bool;
    fn mark_global(&mut self, _: u64);
    fn global_offset(&self) -> u64;

    fn is_unknown(&self) -> bool;

    fn add_refs(&mut self, _: Vec<Self::SSARef>);
    fn refs(&self) -> ::std::slice::Iter<Self::SSARef>;
}

/// Trait that describes variable bindings for a function.
pub trait RBindings {
    type BTy: RBind;
    type Idx: Clone + fmt::Debug + Eq + Ord + Hash + From<usize>;

    fn new() -> Self;
    fn insert(&mut self, _: Self::BTy) -> Self::Idx;

    fn binding(&self, _: &Self::Idx) -> Option<&Self::BTy>;
    fn binding_mut(&mut self, _: &Self::Idx) -> Option<&mut Self::BTy>;

    fn bindings(&self) -> RBinds<Self::BTy>;
    fn bindings_mut(&mut self) -> RBindsMut<Self::BTy>;
}

#[derive(Debug)]
pub struct RBinds<'a, T: 'a + RBind> {
    binds: ::std::slice::Iter<'a, T>,
}

impl<'a, T: 'a + RBind> Iterator for RBinds<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        self.binds.next()
    }
}

#[derive(Debug)]
pub struct RBindsMut<'a, T: 'a + RBind> {
    binds: ::std::slice::IterMut<'a, T>,
}

#[derive(Clone, Debug)]
pub struct RadecoBindings<T: RBind> {
    binds: Vec<T>,
}

impl<BTy: RBind> Index<usize> for RadecoBindings<BTy> {
    type Output = BTy;
    fn index(&self, index: usize) -> &Self::Output {
        self.binding(&index).unwrap_or_else(|| {
            radeco_err!("Binding not found, aborting...");
            process::abort();
        })
    }
}

impl<BTy: RBind> IndexMut<usize> for RadecoBindings<BTy> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.binding_mut(&index).unwrap_or_else(|| {
            radeco_err!("Binding not found, aborting...");
            process::abort();
        })
    }
}

impl<BTy: RBind> RBindings for RadecoBindings<BTy> {
    type BTy = BTy;
    type Idx = usize;

    fn new() -> Self {
        RadecoBindings { binds: Vec::new() }
    }

    fn insert(&mut self, bind: BTy) -> usize {
        self.binds.push(bind);
        self.binds.len() - 1
    }

    fn binding(&self, idx: &usize) -> Option<&BTy> {
        self.binds.get(*idx)
    }

    fn binding_mut(&mut self, idx: &usize) -> Option<&mut BTy> {
        self.binds.get_mut(*idx)
    }

    fn bindings(&self) -> RBinds<BTy> {
        RBinds {
            binds: self.binds.iter(),
        }
    }

    fn bindings_mut(&mut self) -> RBindsMut<BTy> {
        RBindsMut {
            binds: self.binds.iter_mut(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Binding<T: fmt::Debug + Clone> {
    // Optional name used to represent this binding.
    pub named: Option<String>,
    // Type of binding.
    vloc: VarLocation,
    dty: VarDataType,
    // Node indices in the function ssa that corresponds to this binding.
    ssa_refs: Vec<T>,
    set: HashSet<VarSet>,
}

impl<'a, T: 'a + RBind> Iterator for RBindsMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<&'a mut T> {
        self.binds.next()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarLocation {
    Register { name: String },
    Memory(MemoryRegion),
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MemoryRegion {
    // Base, offset for function local.
    FunctionLocal { base: usize, offset: i64 },
    Stack,
    Global { offset: u64 },
    Heap,
    Unknown,
}

// Represents the set to which a variable belongs to in the call summary
// information.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum VarSet {
    Argument,
    Local,
    Modified,
    Preserved,
    Returned,
}

#[derive(Clone, Copy, Debug)]
pub enum VarDataType {
    Unknown(u64),
    Int,
    IntRef,
    Char,
    CharRef,
    Float,
    FloatRef,
    Double,
    DoubleRef,
}

#[derive(Clone, Debug)]
pub struct LocalInfo {
    pub base: usize,
    pub offset: i64,
}

impl<T: Clone + fmt::Debug> Default for Binding<T> {
    fn default() -> Binding<T> {
        Binding {
            named: None,
            vloc: VarLocation::Unknown,
            dty: VarDataType::Unknown(64),
            ssa_refs: Vec::new(),
            set: HashSet::new(),
        }
    }
}

impl<T: Clone + fmt::Debug> RBind for Binding<T> {
    type SSARef = T;

    fn is_argument(&self) -> bool {
        self.set.contains(&VarSet::Argument)
    }

    fn is_local(&self) -> bool {
        self.set.contains(&VarSet::Local)
    }

    fn is_modified(&self) -> bool {
        self.set.contains(&VarSet::Modified)
    }

    fn is_preserved(&self) -> bool {
        self.set.contains(&VarSet::Preserved)
    }

    fn is_return(&self) -> bool {
        self.set.contains(&VarSet::Returned)
    }

    fn mark_argument(&mut self) {
        self.set.insert(VarSet::Argument);
    }

    fn mark_modified(&mut self) {
        self.set.insert(VarSet::Modified);
    }

    fn mark_preserved(&mut self) {
        self.set.insert(VarSet::Preserved);
    }

    fn mark_return(&mut self) {
        self.set.insert(VarSet::Returned);
    }

    fn name(&self) -> String {
        self.named.clone().unwrap_or_else(String::new)
    }

    fn set_name(&mut self, name: String) {
        self.named = Some(name);
    }

    fn is_register(&self) -> bool {
        match self.vloc {
            VarLocation::Register { .. } => true,
            _ => false,
        }
    }

    fn mark_register(&mut self, name: String) {
        self.vloc = VarLocation::Register { name: name };
    }

    fn mark_memory(&mut self) {
        self.vloc = VarLocation::Memory(MemoryRegion::Unknown);
    }

    fn is_fn_local(&self) -> bool {
        match self.vloc {
            VarLocation::Memory(MemoryRegion::FunctionLocal { .. }) => true,
            _ => false,
        }
    }

    fn mark_fn_local(&mut self, base: usize, offset: i64) {
        self.vloc = VarLocation::Memory(MemoryRegion::FunctionLocal {
            base: base,
            offset: offset,
        });
    }

    fn local_info(&self) -> LocalInfo {
        if let VarLocation::Memory(MemoryRegion::FunctionLocal { base, offset }) = self.vloc {
            LocalInfo {
                base: base,
                offset: offset,
            }
        } else {
            panic!()
        }
    }

    fn is_stack(&self) -> bool {
        if let VarLocation::Memory(MemoryRegion::Stack) = self.vloc {
            true
        } else {
            false
        }
    }

    fn mark_stack(&mut self) {
        self.vloc = VarLocation::Memory(MemoryRegion::Stack);
    }

    fn is_global(&self) -> bool {
        match self.vloc {
            VarLocation::Memory(MemoryRegion::Global { .. }) => true,
            _ => false,
        }
    }

    fn mark_global(&mut self, offset: u64) {
        self.vloc = VarLocation::Memory(MemoryRegion::Global { offset: offset });
    }

    fn global_offset(&self) -> u64 {
        if let VarLocation::Memory(MemoryRegion::Global { offset }) = self.vloc {
            offset
        } else {
            panic!()
        }
    }

    fn is_unknown(&self) -> bool {
        match self.vloc {
            VarLocation::Unknown => true,
            _ => false,
        }
    }

    fn add_refs(&mut self, refs: Vec<T>) {
        self.ssa_refs.extend(refs);
    }

    fn refs(&self) -> ::std::slice::Iter<T> {
        self.ssa_refs.iter()
    }
}
