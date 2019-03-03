// build.rs
// This build script is used to recognize radeco-lib version at
// compile from the Cargo.lock file and save it in the
// VERSION_STR environment variable to be retrieved at runtime.
extern crate toml;

use std::fs::read_to_string;
use std::path::Path;

// Refer: https://docs.rs/built/0.3.0/src/built/lib.rs.html#264
fn parse_dependencies(lock_toml_buf: &str) -> Vec<(String, String)> {
    let lock_toml: toml::Value = lock_toml_buf.parse().unwrap();
    let mut deps = Vec::new();

    // Get the table of [[package]]s. This is the deep list of dependencies and
    // dependencies of dependencies.
    for package in lock_toml["package"].as_array().unwrap() {
        let package = package.as_table().unwrap();
        deps.push((
            package.get("name").unwrap().as_str().unwrap().to_owned(),
            package.get("version").unwrap().as_str().unwrap().to_owned(),
        ));
    }
    deps.sort();
    deps
}

fn main() {
    let radeco_version = env!("CARGO_PKG_VERSION").to_string();
    let mut version_str = format!("radeco - v{}", radeco_version);

    let manifest_dir_path = env!("CARGO_MANIFEST_DIR").to_string();
    let cargo_lock_path = Path::new(&manifest_dir_path).join("Cargo.lock");

    if cargo_lock_path.exists() {
        let lock_buf = read_to_string(cargo_lock_path).ok().unwrap();
        let deps = parse_dependencies(&lock_buf);
        for &(ref crate_name, ref crate_version) in deps.iter() {
            if crate_name == "radeco-lib" {
                version_str = format!("{}, radeco-lib - v{}", version_str, crate_version);
            }
        }
    }
    println!("cargo:rustc-env=VERSION_STR={}", version_str);
}
