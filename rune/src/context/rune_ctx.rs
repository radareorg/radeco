//! Define break;

use petgraph::graph::NodeIndex;
use std::collections::HashMap;

use libsmt::backends::smtlib2::{SMTLib2, SMTProc};
use libsmt::backends::backend::SMTBackend;
use libsmt::logics::qf_abv;
use libsmt::theories::bitvec;

use crate::memory::memory::Memory;

use crate::regstore::regstore::{RegStore, RegStoreAPI};
//use regstore::regfile::RuneRegFile;

use crate::context::context::{Context, ContextAPI, Evaluate, RegisterRead, RegisterWrite, MemoryRead, MemoryWrite};

#[derive(Clone, Debug)]
pub struct RuneContext<Mem, Reg>
    where Mem: Memory,
          Reg: RegStore
{
    ip: u64,
    pub solver: SMTLib2<qf_abv::QF_ABV>,
    regstore: Reg,
    mem: Mem,
    e_old: Option<NodeIndex>,
    e_cur: Option<NodeIndex>,
}

impl<Mem, Reg> Context for RuneContext<Mem, Reg>
    where Mem: Memory<VarRef=NodeIndex>,
          Reg: RegStore<VarRef=NodeIndex>
{
    fn set_e_old(&mut self, i: NodeIndex) {
        self.e_old = Some(i);
    }

    fn set_e_cur(&mut self, i: NodeIndex) {
        self.e_cur = Some(i);
    }

    fn e_old(&self) -> NodeIndex {
        assert!(self.e_old.is_some(), "e_old accessed before being set!");
        self.e_old.unwrap()
    }

    fn e_cur(&self) -> NodeIndex {
        assert!(self.e_cur.is_some(), "e_cur accessed before being set!");
        self.e_cur.unwrap()
    }

    fn ip(&self) -> u64 {
        self.ip
    }

    fn is_symbolic(&self) -> bool {
        true
    }

    fn increment_ip(&mut self, by: u64) {
        self.ip += by;
    }

    fn set_ip(&mut self, to: u64) {
        self.ip = to;
    }

    fn define_const(&mut self, c: u64, size: usize) -> NodeIndex {
        self.solver.new_const(bitvec::OpCodes::Const(c, size))
    }

    fn alias_of(&self, reg: String) -> Option<String> {
        self.regstore.get_reg_entry(&reg).alias.clone()
    }

    fn solve<S: SMTProc>(&mut self, p: &mut S) -> HashMap<NodeIndex, u64> {
        self.solver.solve(p).expect("No satisfying solution.")
    }

    fn var_named<T: AsRef<str>>(&self, _var: T) -> Option<NodeIndex> {
        None
    }
}

impl<Mem, Reg> RegisterRead for RuneContext<Mem, Reg>
where Mem: Memory<VarRef=NodeIndex>,
      Reg: RegStore<VarRef=NodeIndex>
{
    type VarRef = NodeIndex;

    fn reg_read<T: AsRef<str>>(&mut self, reg: T) -> NodeIndex {
        self.regstore.read(reg.as_ref(), &mut self.solver)
    }
}

impl<Mem, Reg> RegisterWrite for RuneContext<Mem, Reg>
where Mem: Memory<VarRef=NodeIndex>,
      Reg: RegStore<VarRef=NodeIndex>
{
    type VarRef = NodeIndex;

    fn reg_write<T: AsRef<str>>(&mut self, reg: T, source: NodeIndex) {
        let e_old = self.regstore.write(reg.as_ref(), source);
        // XXX: THIS IS A HACK!
        // IF NOT REG
        if !reg.as_ref().to_owned().ends_with('f') {
            self.e_old = e_old;
            self.e_cur = Some(source);
        }
    }
}

impl<Mem, Reg> MemoryRead for RuneContext<Mem, Reg>
where Mem: Memory<VarRef=NodeIndex>,
      Reg: RegStore<VarRef=NodeIndex>
{
    type VarRef = NodeIndex;

    fn mem_read(&mut self, addr: NodeIndex, read_size: usize) -> NodeIndex {
        // Assert read size is multiple of 8
        assert_eq!(read_size%8, 0, "Read Size is not divisible by 8");
        self.mem.read(addr, read_size, &mut self.solver)
    }
}

impl<Mem, Reg> MemoryWrite for RuneContext<Mem, Reg>
where Mem: Memory<VarRef=NodeIndex>,
      Reg: RegStore<VarRef=NodeIndex>
{
    type VarRef = NodeIndex;

    fn mem_write(&mut self, addr: NodeIndex, data: NodeIndex, write_size: usize) {
        // Assert write size is multiple of 8
        assert_eq!(write_size%8, 0, "Write Size is not divisible by 8");
        self.mem.write(addr, data, write_size, &mut self.solver);
    }
}

impl<Mem, Reg> Evaluate for RuneContext<Mem, Reg>
where Mem: Memory<VarRef=NodeIndex>,
      Reg: RegStore<VarRef=NodeIndex>
{
    type VarRef = NodeIndex;
    type IFn = qf_abv::QF_ABV_Fn;

    fn eval<T, Q>(&mut self, smt_fn: T, operands: Q) -> Self::VarRef
        where T: Into<Self::IFn>,
              Q: AsRef<[Self::VarRef]>
    {
        // TODO: Add extract / concat to ensure that the registers are of compatible
        // sizes for operations.
        self.solver.assert(smt_fn, operands.as_ref())
    }
}

impl<Mem, Reg> ContextAPI for RuneContext<Mem, Reg>
where Mem: Memory<VarRef=NodeIndex>,
      Reg: RegStore<VarRef=NodeIndex> + RegStoreAPI
{
    fn set_reg_as_const<T: AsRef<str>>(&mut self, reg: T, val: u64) -> NodeIndex {
        if let Some(cval) = self.regstore.get_reg_ref(reg.as_ref()) {
            cval
        } else {
            let cval = self.define_const(val, 64);
            self.regstore.set_reg(reg.as_ref(), cval);
            cval
        }
    }

    fn set_reg_as_sym<T: AsRef<str>>(&mut self, reg: T) -> NodeIndex {
        let sym = self.solver.new_var(Some(reg.as_ref()), qf_abv::bv_sort(64));
        self.regstore.set_reg(reg.as_ref(), sym);
        // self.syms.insert(reg.as_ref().to_owned(), sym);

        sym
    }

    fn set_mem_as_const(&mut self, addr: u64, val: u64, write_size: usize) -> NodeIndex {
        // Assert that memory var is in chunks of 8
        assert_eq!(write_size%8, 0, "Write size is not divisible by 8!");

        let addr = self.define_const(addr, 64);
        let cval = self.define_const(val, write_size);
        self.mem_write(addr, cval, write_size);

        cval
    }

    fn set_mem_as_sym(&mut self, addr: u64, write_size: usize) -> NodeIndex {
        // Assert that memory var is in chunks of 8
        assert_eq!(write_size%8, 0, "Write size is not divisible by 8!");

        let key = format!("mem_{}_{}", addr, write_size/8);
        let sym = self.solver.new_var(Some(&key), qf_abv::bv_sort(write_size));
        let addr = self.define_const(addr, 64);

        self.mem_write(addr, sym, write_size);
        // self.syms.insert(key, sym);

        sym
    }

    fn zero_registers(&mut self) {
        let cval = Some(self.define_const(0, 64));
        for reg in &mut self.regstore.get_regs() {
            if reg.is_none() {
                *reg = cval;
            }
        }
    }

    fn registers(&self) -> Vec<String> {
        unimplemented!();
    }
}

impl<Mem, Reg> RuneContext<Mem, Reg>
where Mem: Memory,
      Reg: RegStore
{
    pub fn new(ip: Option<u64>,
               mem: Mem,
               regstore: Reg,
               solver: SMTLib2<qf_abv::QF_ABV>)
               -> RuneContext<Mem, Reg> {
        RuneContext {
            ip: ip.unwrap_or(0),
            mem: mem,
            regstore: regstore,
            solver: solver,
            e_old: None,
            e_cur: None,
        }
    }
}

mod test {
    use super::*;

    use libsmt::logics::qf_abv;
    use libsmt::backends::smtlib2::SMTLib2;

    use crate::memory::seg_mem::SegMem;
    use crate::regstore::regfile::RuneRegFile;

    use r2api::structs::Endian;

    // #[test]
    fn testing_memory_my_dude() {
        let mut lreginfo = Default::default();
        let regstore = RuneRegFile::new(&mut lreginfo);

        let mut mem = SegMem::new(64, Endian::Big);
		let mut smt = SMTLib2::new(Some(qf_abv::QF_ABV));
		mem.init_memory(&mut smt);

        let _ctx = RuneContext::new(Some(0x9000), mem, regstore, smt);
    }
}
