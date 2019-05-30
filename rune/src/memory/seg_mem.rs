use petgraph::graph::NodeIndex;
use petgraph::graph::Graph;
use petgraph::Direction;

use std::collections::BTreeMap;
use std::cmp::Ordering;

use libsmt::backends::smtlib2::{SMTLib2, SMTProc};
use libsmt::backends::backend::SMTBackend;
use libsmt::logics::qf_abv;
use libsmt::logics::qf_abv::QF_ABV_Fn::BVOps;
use libsmt::theories::bitvec::OpCodes::*;
use libsmt::theories::core::OpCodes::*;
use libsmt::theories::{integer, array_ex, bitvec, core};
use libsmt::backends::z3::Z3;

use r2api::structs::Endian;

use memory::memory::Memory;
use utils::utils::simplify_constant;

#[derive(Copy, Clone, Debug)]
pub struct MemRange {
    start: u64,
    end: u64
}

impl MemRange {
    fn new(start: u64, end: u64) -> MemRange {
        MemRange {
            start: start,
            end: end,
        }
    }

    fn get_width(&self) -> usize {
        (self.end - self.start) as usize * 8
    }

    fn contains(&self, num: u64) -> bool {
        if num >= self.start && num < self.end {
            true
        } else {
            false
        }
    }
}

impl PartialEq for MemRange {
    fn eq(&self, other:&MemRange) -> bool {
        if self.start == other.start && self.end == other.end {
            true
        } else {
            false
        }
    }
}

impl Eq for MemRange {}

impl PartialOrd for MemRange {
    fn partial_cmp(&self, other: &MemRange) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MemRange {
    fn cmp(&self, other: &MemRange) -> Ordering {
        if self.start == other.start && self.end == other.end {
            Ordering::Equal
        } else if self.start < other.start {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

#[derive(Clone, Debug)]
pub struct MemBlock {
    range: MemRange,
    solver_idx: Option<NodeIndex>,
}

impl MemBlock {
    pub fn new(m_range: MemRange, solver_idx: Option<NodeIndex>) -> MemBlock {
        MemBlock {
            range: m_range,
            solver_idx: solver_idx,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SegMem {
    addr_width: usize,
    endian: Endian,
    segments: BTreeMap<MemRange, MemBlock>,
}

impl SegMem {
    fn read_segment(&mut self, cov: u64, start: u64, end: u64, low: u64, high: u64, width: u64, e_mem: Option<NodeIndex>, solver: &mut SMTLib2<qf_abv::QF_ABV>) -> NodeIndex {
        let shift = cov;
        let ext   = width - (high - low);

        if let Some(mem) = e_mem {
            let int1 = solver.assert(Extract(high - 1, low), &[mem]);
            let int2 = solver.assert(ZeroExtend(ext), &[int1]);
            let int3 = solver.new_const(Const(shift, width as usize));
            let int4 = solver.assert(BvShl, &[int2, int3]);

            int4
        } else {
            let size = high - low;
            let key = format!("mem_{}_{}", start, size/8);

            let int1 = solver.new_var(Some(&key), qf_abv::bv_sort(size as usize));
            // let int2 = solver.assert(ZeroExtend(ext), &[int1]);
            // let int3 = solver.new_const(Const(shift, width as usize));
            // let int4 = solver.assert(BvShl, &[int2, int3]);
            
            let r = MemRange::new(start, end);
            let m = MemBlock::new(r, Some(int1));

            self.segments.insert(r, m);

            int1
        }
    }
}

impl Memory for SegMem {
    type VarRef = NodeIndex;

    fn new(address_width: usize, endian: Endian) -> SegMem {
        SegMem {
            addr_width: address_width, 
            endian: endian,
            segments: BTreeMap::new(),
        }
    }

    fn init_memory(&mut self, solver: &mut SMTLib2<qf_abv::QF_ABV>) {
        self.segments = BTreeMap::new();
    }

    fn read(&mut self, addr: NodeIndex, read_size: usize, solver: &mut SMTLib2<qf_abv::QF_ABV>) -> NodeIndex {
        // Assert that address is valid
        let addr = simplify_constant(addr, solver);

        let read_range = MemRange::new(addr, addr+(read_size/8) as u64);

        let mem = self.segments.clone();
        let mut ranges: Vec<MemRange> = mem.keys().cloned().collect();

        let mut pos = match ranges.binary_search(&&read_range) {
            Ok(0) | Err(0) => 0,
            Ok(pos) | Err(pos) => pos - 1,
        };

        let mut iterator = ranges.split_at(pos).1.iter().peekable();

        let width = read_range.get_width() as u64;
        let START = read_range.start;
        let END   = read_range.end;

        let mut low:  u64;
        let mut high: u64;

        let mut result = solver.new_const(Const(0, width as usize));

        let mut ptr = START;
        let mut cov = 0;

        while ptr != END {
            cov = (ptr - START)*8;
            if let Some(&current) = iterator.peek() {
                if current.start == START && current.end == END {
                    // current is the mem requested
                    let node     = mem.get(&current).unwrap();
                    let node_idx = node.solver_idx.unwrap();

                    result = node_idx;

                    ptr = END;
                } else if current.start == ptr && current.end <= END {
                    // Use current
                    let node     = mem.get(&current).unwrap();
                    let node_idx = node.solver_idx.unwrap();

                    result = solver.assert(BvOr, &[result, node_idx]);

                    ptr = current.end;
                    iterator.next();
                } else if current.contains(ptr) && current.contains(END) {
                    // extract entire
                    let node     = mem.get(&current).unwrap();
                    let node_idx = node.solver_idx.unwrap();

                    low  = (ptr - current.start)*8;
                    high = (END - current.start)*8;

                    let int = self.read_segment(cov, ptr, END,
                                                low, high, width,
                                                Some(node_idx), solver);
                    result  = solver.assert(BvOr, &[result, int]);

                    ptr = END;
                    iterator.next();
                } else if current.contains(ptr) && !current.contains(END) {
                    // extract till end of current
                    let node     = mem.get(&current).unwrap();
                    let node_idx = node.solver_idx.unwrap();

                    low  = (ptr - current.start)*8;
                    high = (current.end - current.start)*8;

                    let int = self.read_segment(cov, ptr, current.end,
                                                low, high, width,
                                                Some(node_idx), solver);
                    result  = solver.assert(BvOr, &[result, int]);

                    ptr = current.end;
                    iterator.next();
                } else if current.start < END && current.end >= END {
                    // create free var till current.start
                    low  = 0;
                    high = (current.start - ptr)*8;
                    
                    let int = self.read_segment(cov, ptr, current.start,
                                                low, high, width,
                                                None, solver);
                    result  = solver.assert(BvOr, &[result, int]);

                    ptr = current.start;
                } else {
                    // create free var till end
                    low  = 0;
                    high = (END - ptr)*8;

                    let int = self.read_segment(cov, ptr, END,
                                                low, high, width,
                                                None, solver);
                    result  = solver.assert(BvOr, &[result, int]);

                    ptr = END;
                }
            } else {
                // create free var till end
                low  = 0;
                high = (END - ptr)*8;

                let int = self.read_segment(cov, ptr, END,
                                            low, high, width,
                                            None, solver);
                result  = solver.assert(BvOr, &[result, int]);

                ptr = END;
            }
        }
        println!("{}", solver.generate_asserts());
        result
    }

    fn write(&mut self, addr: NodeIndex, data: NodeIndex, write_size: usize, solver: &mut SMTLib2<qf_abv::QF_ABV>) {
        let idx = self.read(addr, write_size, solver);
        solver.assert(Cmp, &[idx, data]);
    }
}

mod test {
    use super::*;

    #[test]
    fn check_read() {
        let mut z3: Z3 = Default::default(); 
        let mut solver = SMTLib2::new(Some(qf_abv::QF_ABV));
        let mut mem = SegMem::new(64, Endian::Big);

        let addr = solver.new_const(Const(0x9000, 64));
        let data = solver.new_const(Const(0x70, 8));
        mem.write(addr, data, 8, &mut solver);

        let addr = solver.new_const(Const(0x9001, 64));
        let data = solver.new_const(Const(0x79, 8));
        mem.write(addr, data, 8, &mut solver);

        let addr = solver.new_const(Const(0x9000, 64));
        let var  = mem.read(addr, 16, &mut solver);

        let c   = solver.new_const(Const(0x7970, 16));
        let cmp = solver.assert(Cmp, &[var, c]);
        
        println!("{}", solver.generate_asserts());

        panic!("ZZ");
    }
}


