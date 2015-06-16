//! Module that provides interaction with radare2.
//! Uses r2pipe.
#![allow(dead_code)]
use r2pipe::R2Pipe;
use rustc_serialize::json::Json;

struct R2 {
    pipe: R2Pipe,
    readin: String,
}

struct JsonWrapper<T> {
    json: Json,
    data: T,
}

// fn send and recv allow users to send their own commands, i.e. The ones that are not currently
// abstracted by the R2 API.
// Ideally, all commonly used commands must be supported for easier use.

impl R2 {
    pub fn new(path: &str) -> R2 {
        let pipe = open_pipe!(path).unwrap();
        R2 { pipe: pipe, readin: String::new() }
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

    pub fn get_fn_list(&mut self) -> Json {
        self.send("aflj");
        self.recv_json()
    }

}

mod test {
    use frontend::r2::R2;

    #[test]
    fn test1() {
        let mut r2 = R2::new("/bin/ls");
        r2.analyze();
        println!("{}", r2.get_fn_list().as_array().unwrap()[0].find_path(&["name"]).unwrap());
        //println!("{}", r2.recv());
    }
}
