//! Module that parses arguments and initializes radeco
use errors::ArgError;
use std::io::Read;
use std::process::Command;

use docopt::Docopt;
use radeco_lib;
use structs;

static USAGE: &'static str = "
radeco. The radare2 decompiler.
Usage:
  radeco <file>
  radeco [options] [<file>]
  radeco run [options] [<file>]

Options:
  --config <json_file>     Run decompilation using json rules file.
  --make-config <file>     Wizard to make the JSON config.
  -a --address=<addr>      Address of function to decompile.
  -e --esil=<esil_expr>    Evaluate given esil expression.
  -o --output=<output>     Specify output directory.
  -p --pipeline=<pipe>     Stages in the pipeline. Comma separated values.
                           Prefix the string with '=' (such as =ssa)
                           to obtain the output the stage.
                           Valid values: c,r2,ssa,cfg,const,dce,verify,svg,png
  -q --quiet               Display silent output.
  -s --shell               Run interactive prompt.
  -v --version             Show version.
  -h --help                Show this screen.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_esil_expr: Option<Vec<String>>,
    arg_file: Option<String>,
    cmd_run: bool,
    flag_address: Option<String>,
    flag_config: Option<String>,
    flag_help: bool,
    flag_make_config: bool,
    flag_shell: bool,
    flag_quiet: bool,
    flag_version: bool,
    flag_output: Option<String>,
    flag_esil: Option<Vec<String>>,
    flag_pipeline: Option<String>,
}

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub type ArgResult<T> = Option<Result<T, ArgError>>;

fn make_config() -> structs::Input {
    structs::input_builder()
}

#[allow(dead_code)]
pub struct Radeco {
    bin_name: Option<String>,
    esil: Option<Vec<String>>,
    addr: String,
    name: Option<String>,
    outpath: String,
    stages: Vec<usize>,
    quiet: bool,
    runner: Option<radeco_lib::utils::Runner>,
    outmodes: Option<Vec<u16>>,
    post_runner: Option<Vec<PostRunner>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PostRunner {
    Genpng,
    Gensvg,
}

macro_rules! radeco_out {
	($x: expr, $b: expr) => {
		if $b {
			println!("{}", $x);
		}
	}
}

impl Radeco {
    pub fn new(input: structs::Input, post: Option<Vec<PostRunner>>) -> ArgResult<Radeco> {
        let runner = input.validate();
        if runner.is_err() {
            return Some(Err(runner.err().unwrap()));
        }

        let radeco = Radeco {
            bin_name: input.bin_name.clone(),
            esil: input.esil.clone(),
            addr: input.addr.clone().unwrap(),
            name: input.name.clone(),
            outpath: input.outpath.clone().unwrap(),
            stages: input.stages.clone(),
            quiet: input.quiet.clone().unwrap(),
            runner: runner.ok(),
            outmodes: input.outmodes.clone(),
            post_runner: post,
        };

        Some(Ok(radeco))
    }

    pub fn init() -> ArgResult<Radeco> {
        let args: Args = Docopt::new(USAGE).and_then(|d| d.decode()).unwrap_or_else(|e| e.exit());

        if args.flag_version {
            println!("Version: {:?}", VERSION);
            return None;
        } else if args.flag_help {
            println!("{}", USAGE);
            return None;
        }

        let mut post_runner = Vec::<PostRunner>::new();
        // Make config file and run radeco with it
        if args.flag_make_config {
            let input = make_config();
            input.spew_json();
            // TODO: populate post_runner
            return Radeco::new(input, Some(post_runner));
        }

        // Run from a predefined configuration file
        if args.flag_config.is_some() {
            let filename = args.flag_config.clone().unwrap();
            let input = structs::Input::from_config(filename);
            if input.is_err() {
                return Some(Err(input.err().unwrap()));
            }
            return Radeco::new(input.unwrap(), Some(post_runner));
        }

        let mut input = structs::Input::defaults();

        if args.arg_file.is_some() {
            input.bin_name = args.arg_file;
        }
        if args.flag_address.is_some() {
            input.addr = args.flag_address;
        }
        if args.flag_esil.is_some() {
            input.esil = args.flag_esil;
        }
        input.quiet = Some(!args.flag_quiet);
        if args.flag_output.is_some() {
            input.outpath = args.flag_output;
        }

        if args.flag_pipeline.is_some() {
            let pipe = args.flag_pipeline.unwrap();
            let mut _tmp_stages = Vec::<usize>::new();
            let mut exclude = Vec::<u16>::new();
            for (i, stage) in pipe.split(',').enumerate() {
                let j = match stage.chars().nth(0).unwrap() {
                    '-' => {
                        exclude.push(i as u16);
                        1
                    }
                    '+' => 1,
                    _ => 0,
                };

                // Try and match with post runner stages
                match &stage[j..] {
                    "png" => {
                        post_runner.push(PostRunner::Genpng);
                        continue;
                    }
                    "svg" => {
                        post_runner.push(PostRunner::Gensvg);
                        continue;
                    }
                    _ => (),
                }

                let n = match &stage[j..] {
                    "r2" => 0,
                    "esil" => 1,
                    "cfg" => 2,
                    "ssa" => 3,
                    "const" => 4,
                    "dce" => 5,
                    "verify" => 6,
                    "c" => 7,
                    _ => {
                        let e = ArgError::InvalidArgument(stage[j..].to_owned());
                        return Some(Err(e));
                    }
                };
                _tmp_stages.push(n);
            }

            let mut _tmp_out = Vec::<u16>::new();
            for i in (0..(pipe.len() - 1) as u16) {
                if exclude.contains(&i) {
                    continue;
                }
                _tmp_out.push(i);
            }
            input.stages = _tmp_stages;
            input.outmodes = Some(_tmp_out);
        }

        Radeco::new(input, Some(post_runner))
    }

    pub fn run(&mut self) -> Result<(), String> {
        match self.runner.as_mut() {
            Some(ref mut runner) => {
                radeco_out!("[*] Starting radeco with config: ", runner.is_verbose());
                radeco_out!(runner, runner.is_verbose());
                runner.run()
            }
            None => panic!("Uninitialized instance of radeco!"),
        }
        Ok(())
    }

    pub fn output(&mut self) {
        match self.runner.as_mut() {
            Some(ref mut runner) => {
                radeco_out!("[*] Writing output", runner.is_verbose());
                runner.output(self.outmodes.clone());
            }
            None => panic!("Uninitialized instance of radeco!"),
        }

        // Post processing phases defined in radeco
        // These can be generation of svg, png etc
        for phase in self.post_runner.as_ref().unwrap() {
            match *phase {
                PostRunner::Genpng | PostRunner::Gensvg => {
                    let format = if *phase == PostRunner::Genpng {
                        "png"
                    } else {
                        "svg"
                    };
                    radeco_out!("[*] Generating svg",
                                self.runner.as_ref().unwrap().is_verbose());
                    let mut cmd = String::new();
                    let mut target = self.outpath.clone();
                    target.push_str("/");
                    target.push_str(&self.name.as_ref().unwrap());
                    target.push_str("/*.dot");
                    cmd.push_str(&format!("dot -T{} -O ", format));
                    cmd.push_str(&target);

                    let _ = Command::new("sh").arg("-c").arg(cmd).output().unwrap();
                }
            }
        }
    }
}
