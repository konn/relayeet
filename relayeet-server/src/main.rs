use std::env;
use std::path::{Path, PathBuf};

fn main() {
    let mut args: Vec<_> = env::args().collect();
    let prog = args[0].clone();
    if args.len() > 2 {
        panic!(format!("Usage: {} [config.yaml]", prog))
    } else {
        let config_yaml = PathBuf::from(args.pop().unwrap_or("config.yaml".to_string()));
    }
}
