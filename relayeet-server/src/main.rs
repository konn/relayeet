extern crate relayeet_server;
extern crate subscribe_chan;

use relayeet_server::*;

use std::env;
use std::path::PathBuf;
use subscribe_chan as chan;

fn main() {
    let mut args: Vec<_> = env::args().collect();
    println!("args: {:#?}", args);
    let prog = args[0].clone();
    if args.len() > 2 {
        panic!(format!("Usage: {} [config.yaml]", prog))
    }
    let config_yaml = PathBuf::from(if args.len() == 2 {
        args.pop().unwrap()
    } else {
        "config.yaml".to_string()
    });
    let config = Config::parse_yaml(config_yaml);
    println!("{:#?}", config);
    let (s, p) = chan::channel();
    let r = p.subscribe();
    s.send(1);
    println!("{:?}", r.recv());
    let r2 = p.subscribe();
    s.send(2);
    println!("{:?}", r2.recv());
    println!("{:?}", r.recv());
}
