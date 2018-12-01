use syntect::easy::HighlightLines;
use syntect::parsing::SyntaxSet;
use syntect::highlighting::{ThemeSet, Style};
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};

lazy_static! {
	static ref SYNTAX_SET : SyntaxSet = {
		SyntaxSet::load_defaults_newlines()
	};
	static ref THEME_SET: ThemeSet = {
		ThemeSet::load_defaults()
	};
}

pub fn print_highlighted(code : &str) {
	let syntax = SYNTAX_SET.find_syntax_by_extension("rs").unwrap();
	let mut h = HighlightLines::new(syntax, &THEME_SET.themes["base16-ocean.dark"]);
	for line in LinesWithEndings::from(code) {
	    let ranges: Vec<(Style, &str)> = h.highlight(line, &SYNTAX_SET);
	    let escaped = as_24_bit_terminal_escaped(&ranges[..], true);
	    println!("{}", escaped);
	}
}
