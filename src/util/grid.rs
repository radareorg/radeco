#![allow(dead_code)]

use std::ops::Index;

pub struct Grid<ColT, RowT, CellT> {
	recompute: Box<Fn(&mut ColT, &mut RowT) -> CellT>,
	rowdata: Vec<RowT>,
	columns: Vec<(ColT, Vec<CellT>)>
}

impl<ColT, RowT, CellT> Grid<ColT, RowT, CellT> {
	pub fn new(recompute: Box<Fn(&mut ColT, &mut RowT) -> CellT>) -> Self {
		Grid { recompute: recompute, rowdata: Vec::new(), columns: Vec::new() }
	}

	pub fn push_row(&mut self, mut rowdata: RowT) {
		// maybe if rowdata was pushed before the loop, we could
		// pass a reference with longer lifetime to recompute()
		for &mut (ref mut columndata, ref mut celldata) in &mut self.columns {
			celldata.push((self.recompute)(columndata, &mut rowdata));
		}
		self.rowdata.push(rowdata);
	}

	pub fn push_column(&mut self, mut columndata: ColT) {
		// maybe if columndata was pushed before the loop, we could
		// pass a reference with longer lifetime to recompute()
		let rc = &*self.recompute;
		let celldata: Vec<_> = self.rowdata.iter_mut().map(
			|rowdata| rc(&mut columndata, rowdata)
		).collect();
		self.columns.push((columndata, celldata));
	}

	pub fn swap_remove_row(&mut self, colindex: usize) {
		self.columns.swap_remove(colindex);
	}

	pub fn swap_remove_column(&mut self, rowindex: usize) {
		self.rowdata.swap_remove(rowindex);
		for &mut (_, ref mut celldata) in &mut self.columns {
			celldata.swap_remove(rowindex);
		}
	}
}

impl<ColT, RowT, CellT> Index<(usize, usize)> for Grid<ColT, RowT, CellT> {
    type Output = CellT;

    fn index(&self, index: (usize, usize)) -> &CellT {
		&self.columns[index.0].1[index.1]
    }
}
