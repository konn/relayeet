#[macro_use]
extern crate serde_derive;

extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate serde_json;

pub mod config;
pub use self::config::*;
pub mod serdestream;
pub use self::serdestream::*;
