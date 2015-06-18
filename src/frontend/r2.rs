//! Module that provides interaction with radare2.
//! Uses r2pipe.
#![allow(dead_code)]
use r2pipe::R2Pipe;

use rustc_serialize::json::{Json, ToJson};
use rustc_serialize::json;
use std::collections::BTreeMap;

use super::structs::{FunctionInfo};

pub struct R2 {
    pipe: R2Pipe,
    readin: String,
}

pub trait Search {
    fn get_values(&self, key: &[&str], deep: bool) -> Option<Json>;
}

impl Search for Json {
    fn get_values(&self, key: &[&str], deep: bool) -> Option<Json> {
        match *self {
            Json::Array(ref arr) => {
                let mut tmp = Vec::new();
                for x in arr.iter() {
                    x.get_values(key, deep)
                     .map_or((), |e| tmp.push(e));
                }
                if tmp.len() > 0 {
                    return Some(tmp.to_json());
                }
            },
            
            Json::Object(ref obj) => {
                let mut result: BTreeMap<String, Json> = BTreeMap::new();
                for k in key {
                    obj.get((*k)).map_or((), |val| {
                        result.insert((*k).to_string(), val.clone());
                    });
                }

                // If we have found something, return.
                if result.keys().len() > 0 {
                    return Some(result.to_json());
                }

                // If we don't want a deep search, we just return None at this point.
                if !deep {
                    return None;
                }

                // Manually iterate through the keys and check if the values have the key.
                for (_, val) in obj.iter() {
                    let res = val.get_values(key, deep);
                    if res.is_some() {
                        return res;
                    }
                }
            },
            
            _ => return None,
        }
        return None;
    }
}

// fn send and recv allow users to send their own commands, i.e. The ones that are not currently
// abstracted by the R2 API.
// Ideally, all commonly used commands must be supported for easier use.

impl R2 {
    pub fn new(path: &str) -> R2 {
        let pipe = open_pipe!(path).unwrap();
        R2 { pipe: pipe, readin: String::new() }
    }

    // Does some basic configurations (sane defaults).
    pub fn init(&mut self) {
        self.send("e asm.esil = true");
        self.send("e scr.color = false");
        self.send("aa");
        self.flush();
    }

    pub fn close(&mut self) {
        self.send("q!");
    }

    pub fn send(&mut self, cmd: &str) {
        self.readin = self.pipe.cmd(cmd);
    }

    pub fn recv(&mut self) -> String {
        let res = self.readin.clone();
        self.flush();
        res
    }

    pub fn recv_json(&mut self) -> Json {
        let res = self.recv().replace("\n", "");
        Json::from_str(&*res).unwrap()
    }

    pub fn flush(&mut self) {
        self.readin = String::from("");
    }

    pub fn analyze(&mut self) {
        self.send("aa");
        self.flush();
    }

    pub fn get_function(&mut self, func: &str) -> FunctionInfo {
        let cmd = format!("pdfj @ {}", func);
        self.send(&*cmd);
        let raw_json = self.recv();
        // Handle Error here.
        json::decode(&*raw_json).unwrap()
    }

    pub fn get_fn_list(&mut self) -> Json {
        self.send("aflj");
        self.recv_json()
    }
}


mod test {
#![allow(unused_imports)]
    use frontend::r2::{R2, Search};

    #[test]
    fn test_fn_listing() {
        let mut r2 = R2::new("./key");
        r2.analyze();
        let fn_data = r2.get_fn_list();
        assert_eq!(fn_data.get_values(&["addr"], false), None);
        assert!(fn_data.get_values(&["addr"], true).is_some());
        println!("{}", fn_data.get_values(&["name", "offset"], false).unwrap().pretty());
    }

    #[test]
    fn test_fn_disas() {
        let mut r2 = R2::new("./key");
        r2.init();
        let data = r2.get_function("sym.main");
        println!("{:?}", data);
    }
}
