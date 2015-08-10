use web::ui::UI;

struct ReadFromR2 { seek: String }
struct ParseEsil;
struct CFG;
struct SSA;
//struct AnalyzeSSA(Analysis);
struct DCE;
struct CWriter;

#[derive(Default)]
pub struct Pipeline {
	test: usize
}

impl Pipeline {
	pub fn new() -> Pipeline {
		Pipeline { test: 0 }
	}
}

impl UI for Pipeline {}
