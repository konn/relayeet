extern crate relayeet_server;
extern crate subscribe_chan;

use relayeet_server::*;

use std::env;
use std::path::PathBuf;

fn main() {
    let mut args: Vec<_> = env::args().collect();
    let prog = args[0].clone();
    if args.len() > 2 {
        panic!(format!("Usage: {} [config.yaml]", prog))
    }
    let config_yaml = PathBuf::from(if args.len() == 2 {
        args.pop().unwrap()
    } else {
        "config.yaml".to_string()
    });
    let config = Config::parse_yaml(config_yaml).unwrap();
    let server = Server::new(config);
    server.run().unwrap();
}
