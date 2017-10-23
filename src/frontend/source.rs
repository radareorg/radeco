//! Defines the `Source` Trait.

#![deprecated(since="0.2.0", note="Replace with `radeco_source`")]

use std::path::{Path, PathBuf};
use std::fs::{self, File};
use std::io::{Read, Write};

use serde_json;

use r2api::api_trait::R2Api;
use r2pipe::r2::R2;
use r2api::structs::{FunctionInfo, LFlagInfo, LOpInfo, LRegInfo, LSectionInfo, LStringInfo};

pub trait Source {
    fn functions(&mut self) -> Vec<FunctionInfo>;
    fn instructions_at(&mut self, u64) -> Vec<LOpInfo>;
    fn register_profile(&mut self) -> LRegInfo;
    fn flags(&mut self) -> Vec<LFlagInfo>;
    fn section_map(&mut self) -> Vec<LSectionInfo>;

    fn send(&mut self, _: &str) { }

    // Non essential / functions with default implementation.
    fn function_at(&mut self, address: u64) -> Option<FunctionInfo> {
        for f in self.functions() {
            match f.offset {
                Some(off) if address == off => return Some(f),
                _ => {}
            }
        }
        None
    }

    fn function_named(&mut self, fn_name: &str) -> Option<FunctionInfo> {
        for f in self.functions() {
            match f.name {
                Some(ref name) if name == fn_name => return Some(f.clone()),
                _ => {}
            }
        }
        None
    }

    fn instructions_at_fn(&mut self, fn_name: &str) -> Option<Vec<LOpInfo>> {
        if let Some(fn_) = self.function_named(fn_name) {
            if let Some(offset) = fn_.offset {
                return Some(self.instructions_at(offset));
            }
        }
        None
    }

    fn flag_at(&mut self, address: u64) -> Option<LFlagInfo> {
        for flag in self.flags() {
            if flag.offset == address {
                return Some(flag);
            }
        }
        None
    }

    fn section_of(&mut self, address: u64) -> Option<LSectionInfo> {
        for s in self.section_map() {
            let addr = s.vaddr.expect("Invalid section");
            let size = s.size.expect("Invalid section size");
            if address >= addr && address < addr + size {
                return Some(s);
            }
        }
        None
    }
}

// Implementation of `Source` trait for R2.
impl Source for R2 where R2: R2Api {
    fn functions(&mut self) -> Vec<FunctionInfo> {
        let fns = self.fn_list();
        fns.expect("Failed to load funtion info from r2")
    }

    fn instructions_at(&mut self, address: u64) -> Vec<LOpInfo> {
        if let Ok(fn_info) = self.function(&format!("{}", address)) {
            fn_info.ops.unwrap_or_default()
        } else {
            Vec::new()
        }
    }

    fn register_profile(&mut self) -> LRegInfo {
        self.reg_info().expect("Failed to load register profile")
    }

    fn flags(&mut self) -> Vec<LFlagInfo> {
        self.flag_info().expect("Failed to load flags from r2")
    }

    fn section_map(&mut self) -> Vec<LSectionInfo> {
        self.sections().expect("Failed to get section info from r2")
    }

    fn send(&mut self, s: &str) {
        self.send(s);
    }
}

#[derive(Clone, Debug)]
/// File source is used to load information from json files.
/// The files in the directory must be of the form "<base_name>_<suffix>.json". This means that the
/// files must have specific names.
///
/// To construct a FileSource painlessly (and save it for future use), use the `from::From` trait
/// implemented on `R2`.
pub struct FileSource {
    /// Directory where files should be loaded from.
    pub dir: String,
    /// Base name for the sample/example. suffixes will be appended based on what information is
    /// requested for. Hence, files in the directory must have specific names.
    pub base_name: String,
}

impl FileSource {
    fn read_file(&self, suffix: &str) -> String {
        let mut path = PathBuf::from(&self.dir);
        path.push(&format!("{}_{}.json", self.base_name, suffix));
        let mut f = File::open(path).expect("Failed to open file");
        let mut json_str = String::new();
        let _ = f.read_to_string(&mut json_str).expect("Failed to read file");
        json_str
    }

    fn write_file(&mut self, suffix: &str, data: &str) {
        let mut path = PathBuf::from(&self.dir);
        path.push(&format!("{}_{}.json", self.base_name, suffix));
        let mut f = File::create(path).expect("Failed to open file");
        f.write_all(data.to_string()
                        .as_bytes()).expect("Failed to read file");
    }
}

mod suffix {
    pub const FUNCTION_INFO: &'static str = "fn_info";
    pub const INSTRUCTIONS: &'static str = "insts";
    pub const REGISTER: &'static str = "register_profile";
    pub const FLAG: &'static str = "flags";
    pub const SECTION: &'static str = "sections";
    pub const STRING: &'static str = "strings";
}

impl FileSource {
    pub fn open(f: Option<&str>) -> FileSource {
        let path = Path::new(f.unwrap());
        let dir = path.parent().unwrap().to_str().unwrap();
        let base_name = path.file_name().unwrap().to_str().unwrap();
        FileSource {
            dir: dir.to_owned(),
            base_name: base_name.to_owned(),
        }
    }

    pub fn strings(&mut self) -> Vec<LStringInfo> {
        serde_json::from_str(&self.read_file(suffix::STRING)).expect("Failed to decode json")
    }
}

impl Source for FileSource {
    fn functions(&mut self) -> Vec<FunctionInfo> {
        serde_json::from_str(&self.read_file(suffix::FUNCTION_INFO)).expect("Failed to decode json")
    }

    fn instructions_at(&mut self, address: u64) -> Vec<LOpInfo> {
        let suffix = format!("{}_{:#X}", suffix::INSTRUCTIONS, address);
        serde_json::from_str(&self.read_file(&suffix)).expect("Failed to decode json")
    }

    fn register_profile(&mut self) -> LRegInfo {
        serde_json::from_str(&self.read_file(suffix::REGISTER)).expect("Failed to decode json")
    }

    fn flags(&mut self) -> Vec<LFlagInfo> {
        serde_json::from_str(&self.read_file(suffix::FLAG)).expect("Failed to decode json")
    }

    fn section_map(&mut self) -> Vec<LSectionInfo> {
        serde_json::from_str(&self.read_file(suffix::SECTION)).expect("Failed to decode json")
    }
}

impl From<R2> for FileSource {
    fn from(mut r2: R2) -> FileSource {
        let bin_info = r2.bin_info().expect("Failed to load bin_info");
        let fname = bin_info.core.unwrap().file.unwrap();
        let fname = Path::new(&fname).file_stem().unwrap();
        let mut dir = PathBuf::from(".");
        dir.push(&fname);
        fs::create_dir_all(&dir).expect("Failed to create directory");
        let mut fsource = FileSource {
            dir: dir.to_str().unwrap().to_owned(),
            base_name: fname.to_str().unwrap().to_owned(),
        };

        {
            let fns = r2.functions();
            { 
                let json_str = serde_json::to_string(&fns).expect("Failed to encode to json");
                fsource.write_file(suffix::FUNCTION_INFO, &json_str);
            }

            for f in fns {
                let result = r2.instructions_at(f.offset.unwrap());
                let json_str = serde_json::to_string(&result).expect("Failed to encode to json");
                let suffix = format!("{}_{:#X}", suffix::INSTRUCTIONS, f.offset.unwrap());
                fsource.write_file(&suffix, &json_str)
            }

            {
                let reg = r2.register_profile();
                let json_str = serde_json::to_string(&reg).expect("Failed to encode to json");
                fsource.write_file(suffix::REGISTER, &json_str);
            }
            
            {
                let flags = r2.flags();
                let json_str = serde_json::to_string(&flags).expect("Failed to encode to json");
                fsource.write_file(suffix::FLAG, &json_str);
            }

            {
                let sections = r2.section_map();
                let json_str = serde_json::to_string(&sections).expect("Failed to encode to json");
                fsource.write_file(suffix::SECTION, &json_str);
            }

            {
                let strings = r2.strings(false).expect("Unable to load String info from r2");
                let json_str = serde_json::to_string(&strings).expect("Failed to encode to json");
                fsource.write_file(suffix::STRING, &json_str);
            }
        }

        fsource
    }
}
