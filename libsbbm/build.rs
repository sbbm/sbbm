use std::env;
use std::path::Path;

fn main() {
    let target = env::var("TARGET").unwrap();
    if target == "sbbm" {
        let proj_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        let deps_path = Path::new(&proj_dir).parent().unwrap().join("deps");
        println!("cargo:rustc-link-search={}", deps_path.display());
    }
}
