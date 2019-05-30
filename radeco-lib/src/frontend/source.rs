//! Defines the `Source` Trait.

#![deprecated(since="0.2.0", note="Replace with `radeco_source`")]

use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process;

use serde_json;

use r2api::api_trait::R2Api;
use r2api::structs::{FunctionInfo, LFlagInfo, LOpInfo, LRegInfo, LSectionInfo, LStringInfo};
use r2pipe::r2::R2;

pub trait Source {
    fn functions(&mut self) -> Result<Vec<FunctionInfo>, &'static str>;
    fn instructions_at(&mut self, _: u64) -> Result<Vec<LOpInfo>, &'static str>;
    fn register_profile(&mut self) -> Result<LRegInfo, &'static str>;
    fn flags(&mut self) -> Result<Vec<LFlagInfo>, &'static str>;
    fn section_map(&mut self) -> Result<Vec<LSectionInfo>, &'static str>;

    fn send(&mut self, _: &str) {}

    // Non essential / functions with default implementation.
    fn function_at(&mut self, address: u64) -> Option<FunctionInfo> {
        let fs = self.functions().unwrap_or_else(|_e| {
            radeco_err!("Error: {:?}", _e);
            Vec::new()
        });
        for f in fs {
            match f.offset {
                Some(off) if address == off => return Some(f),
                _ => {}
            }
        }
        None
    }

    fn function_named(&mut self, fn_name: &str) -> Option<FunctionInfo> {
        let fs = self.functions().unwrap_or_else(|_e| {
            radeco_err!("Error: {:?}", _e);
            Vec::new()
        });
        for f in fs {
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
                return self.instructions_at(offset).ok();
            }
        }
        None
    }

    fn flag_at(&mut self, address: u64) -> Option<LFlagInfo> {
        let flags = self.flags().unwrap_or_else(|_e| {
            radeco_err!("Error: {:?}", _e);
            Vec::new()
        });
        for flag in flags {
            if flag.offset == address {
                return Some(flag);
            }
        }
        None
    }

    fn section_of(&mut self, address: u64) -> Option<LSectionInfo> {
        let ss = self.section_map().unwrap_or_else(|_e| {
            radeco_err!("Error {:?}", _e);
            Vec::new()
        });
        for s in ss {
            let addr = s.vaddr.unwrap_or_else(|| {
                radeco_err!("Invalid section");
                0
            });
            let size = s.size.unwrap_or_else(|| {
                radeco_err!("Invalid section size");
                0
            });
            if address >= addr && address < addr + size {
                return Some(s);
            }
        }
        None
    }
}

// Implementation of `Source` trait for R2.
impl Source for R2
where
    R2: R2Api,
{
    fn functions(&mut self) -> Result<Vec<FunctionInfo>, &'static str> {
        match self.fn_list() {
            Ok(f) => Ok(f),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to load funtion info from r2")
            }
        }
    }

    fn instructions_at(&mut self, address: u64) -> Result<Vec<LOpInfo>, &'static str> {
        if let Ok(fn_info) = self.function(&format!("{}", address)) {
            Ok(fn_info.ops.unwrap_or_default())
        } else {
            Ok(Vec::new())
        }
    }

    fn register_profile(&mut self) -> Result<LRegInfo, &'static str> {
        match self.reg_info() {
            Ok(r) => Ok(r),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to load register profile")
            }
        }
    }

    fn flags(&mut self) -> Result<Vec<LFlagInfo>, &'static str> {
        match self.flag_info() {
            Ok(f) => Ok(f),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to load flags from r2")
            }
        }
    }

    fn section_map(&mut self) -> Result<Vec<LSectionInfo>, &'static str> {
        match self.sections() {
            Ok(s) => Ok(s),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to get section info from r2")
            }
        }
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
        let mut f = File::open(path).unwrap_or_else(|_e| {
            radeco_err!("Failed to open file");
            radeco_err!("Error: {:?}", _e);
            process::abort();
        });
        let mut json_str = String::new();
        let _ = f.read_to_string(&mut json_str).unwrap_or_else(|_e| {
            radeco_err!("Failed to read file");
            radeco_err!("Error: {:?}", _e);
            process::abort();
        });
        json_str
    }

    fn write_file(&mut self, suffix: &str, data: &str) {
        let mut path = PathBuf::from(&self.dir);
        path.push(&format!("{}_{}.json", self.base_name, suffix));
        let mut f = File::create(path).unwrap_or_else(|_e| {
            radeco_err!("Failed to open file");
            radeco_err!("Error: {:?}", _e);
            process::abort()
        });
        f.write_all(data.to_string().as_bytes())
            .unwrap_or_else(|_e| {
                radeco_err!("Failed to write file");
                radeco_err!("Error: {:?}", _e);
            });
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
        if f.is_none() {
            radeco_err!("Invalid file name");
            process::abort();
        };
        let path = Path::new(f.unwrap());
        if path.parent().is_none() || path.parent().unwrap().to_str().is_none() {
            radeco_err!("Invaild path");
            process::abort();
        }
        if path.file_name().is_none() || path.file_name().unwrap().to_str().is_none() {
            radeco_err!("Invaild path");
            process::abort();
        }
        let dir = path.parent().unwrap().to_str().unwrap();
        let base_name = path.file_name().unwrap().to_str().unwrap();
        FileSource {
            dir: dir.to_owned(),
            base_name: base_name.to_owned(),
        }
    }

    pub fn strings(&mut self) -> Vec<LStringInfo> {
        serde_json::from_str(&self.read_file(suffix::STRING)).unwrap_or_else(|_e| {
            radeco_err!("Failed to decode json\n, Error: {:?}", _e);
            process::abort();
        })
    }
}

impl Source for FileSource {
    fn functions(&mut self) -> Result<Vec<FunctionInfo>, &'static str> {
        match serde_json::from_str(&self.read_file(suffix::FUNCTION_INFO)) {
            Ok(s) => Ok(s),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to decode json")
            }
        }
    }

    fn instructions_at(&mut self, address: u64) -> Result<Vec<LOpInfo>, &'static str> {
        let suffix = format!("{}_{:#X}", suffix::INSTRUCTIONS, address);
        match serde_json::from_str(&self.read_file(&suffix)) {
            Ok(s) => Ok(s),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to decode json")
            }
        }
    }

    fn register_profile(&mut self) -> Result<LRegInfo, &'static str> {
        match serde_json::from_str(&self.read_file(suffix::REGISTER)) {
            Ok(s) => Ok(s),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to decode json")
            }
        }
    }

    fn flags(&mut self) -> Result<Vec<LFlagInfo>, &'static str> {
        match serde_json::from_str(&self.read_file(suffix::FLAG)) {
            Ok(s) => Ok(s),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to decode json")
            }
        }
    }

    fn section_map(&mut self) -> Result<Vec<LSectionInfo>, &'static str> {
        match serde_json::from_str(&self.read_file(suffix::SECTION)) {
            Ok(s) => Ok(s),
            Err(_e) => {
                radeco_err!("{:?}", _e);
                Err("Failed to decode json")
            }
        }
    }
}

impl From<R2> for FileSource {
    fn from(mut r2: R2) -> FileSource {
        if r2.bin_info().is_err() {
            radeco_err!("Failed to load bin_info");
            process::abort();
        }
        let bin_info = r2.bin_info().unwrap();
        let fname = bin_info.core.and_then(|c| c.file).unwrap_or_else(|| {
            radeco_err!("Failed to load bin_info.core");
            process::abort();
        });
        let fname = Path::new(&fname).file_stem().unwrap_or_else(|| {
            radeco_err!("File not found");
            process::abort();
        });
        let mut dir = PathBuf::from(".");
        dir.push(&fname);
        fs::create_dir_all(&dir).unwrap_or_else(|_e| {
            radeco_err!("Error {:?}", _e);
            process::abort();
        });
        if dir.to_str().is_none() || fname.to_str().is_none() {
            radeco_err!("No such Directory or filename");
            process::abort();
        }
        let mut fsource = FileSource {
            dir: dir.to_str().unwrap().to_owned(),
            base_name: fname.to_str().unwrap().to_owned(),
        };

        {
            let fns = r2.functions().unwrap_or_else(|_e| {
                radeco_err!("{:?}", _e);
                Vec::new()
            });
            {
                let json_str = serde_json::to_string(&fns).unwrap_or_else(|_e| {
                    radeco_err!("Error: {:?}", _e);
                    "Invalid json".to_string()
                });
                fsource.write_file(suffix::FUNCTION_INFO, &json_str);
            }

            for f in fns {
                let offset = f.offset.unwrap_or_else(|| {
                    radeco_err!("Offset not found");
                    0
                });
                let result = r2.instructions_at(offset);
                let json_str = serde_json::to_string(&result).unwrap_or_else(|_e| {
                    radeco_err!("Error: {:?}", _e);
                    "Invalid json".to_string()
                });
                let suffix = format!("{}_{:#X}", suffix::INSTRUCTIONS, offset);
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
                let strings = r2
                    .strings(false)
                    .expect("Unable to load String info from r2");
                let json_str = serde_json::to_string(&strings).expect("Failed to encode to json");
                fsource.write_file(suffix::STRING, &json_str);
            }
        }

        fsource
    }
}
