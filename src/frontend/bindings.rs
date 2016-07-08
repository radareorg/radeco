//! Defines structs and methods that describe a bindings.

use std::collections::HashSet;
use std::fmt;

use petgraph::graph::NodeIndex;

#[derive(Clone, Debug, PartialEq)]
pub enum VarLocation {
    Register {
        name: String,
    },
    Memory(MemoryRegion),
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MemoryRegion {
    // Base, offset for function local.
    FunctionLocal {
        base: usize,
        offset: i64,
    },
    Stack,
    Global {
        offset: u64,
    },
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

#[derive(Clone, Debug)]
pub struct Binding<T: fmt::Debug + Clone> {
    // Optional name used to represent this binding.
    named: Option<String>,
    // Type of binding.
    vloc: VarLocation,
    dty: VarDataType,
    // Node indices in the function ssa that corresponds to this binding.
    ssa_refs: Vec<T>,
    set: HashSet<VarSet>,
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
    base: usize,
    offset: i64,
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

impl<T: Clone + fmt::Debug> Binding<T> {
    pub fn is_argument(&self) -> bool {
        self.set.contains(&VarSet::Argument)
    }

    pub fn is_local(&self) -> bool {
        self.set.contains(&VarSet::Local)
    }

    pub fn is_modified(&self) -> bool {
        self.set.contains(&VarSet::Modified)
    }

    pub fn is_preserved(&self) -> bool {
        self.set.contains(&VarSet::Preserved)
    }

    pub fn is_return(&self) -> bool {
        self.set.contains(&VarSet::Returned)
    }

    pub fn mark_argument(&mut self) {
        self.set.insert(VarSet::Argument);
    }

    pub fn mark_local(&mut self) {
        self.set.insert(VarSet::Local);
    }

    pub fn mark_modified(&mut self) {
        self.set.insert(VarSet::Modified);
    }

    pub fn mark_preserved(&mut self) {
        self.set.insert(VarSet::Preserved);
    }

    pub fn mark_return(&mut self) {
        self.set.insert(VarSet::Returned);
    }

    pub fn name(&self) -> String {
        self.named.clone().unwrap_or_else(String::new)
    }

    pub fn is_register(&self) -> bool {
        match self.vloc {
            VarLocation::Register { name: _ } => true,
            _ => false,
        }
    }

    pub fn mark_register(&mut self, name: String) {
        self.vloc = VarLocation::Register { name: name };
    }

    pub fn is_fn_local(&self) -> bool {
        match self.vloc {
            VarLocation::Memory(MemoryRegion::FunctionLocal { base: _, offset: _ }) => true,
            _ => false,
        }
    }

    pub fn mark_fn_local(&mut self, base: usize, offset: i64) {
        self.vloc = VarLocation::Memory(MemoryRegion::FunctionLocal {
            base: base,
            offset: offset,
        });
    }

    pub fn local_info(&self) -> LocalInfo {
        if let VarLocation::Memory(MemoryRegion::FunctionLocal { base, offset }) = self.vloc {
            return LocalInfo {
                base: base,
                offset: offset,
            };
        }
        panic!()
    }

    pub fn is_stack(&self) -> bool {
        if let VarLocation::Memory(MemoryRegion::Stack) = self.vloc {
            true
        } else {
            false
        }
    }

    pub fn mark_stack(&mut self) {
        self.vloc = VarLocation::Memory(MemoryRegion::Stack);
    }

    pub fn is_global(&self) -> bool {
        match self.vloc {
            VarLocation::Memory(MemoryRegion::Global { offset }) => true,
            _ => false,
        }
    }

    pub fn mark_global(&mut self, offset: u64) {
        self.vloc = VarLocation::Memory(MemoryRegion::Global { offset: offset });
    }

    pub fn global_offset(&self) -> u64 {
        if let VarLocation::Memory(MemoryRegion::Global { offset }) = self.vloc {
            offset
        } else {
            panic!()
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self.vloc {
            VarLocation::Unknown => true,
            _ => false
        }
    }

    pub fn add_refs(&mut self, refs: Vec<T>) {
        self.ssa_refs.extend(refs);
    }

    pub fn refs<'a>(&'a self) -> ::std::slice::Iter<'a, T> {
        self.ssa_refs.iter()
    }
}
