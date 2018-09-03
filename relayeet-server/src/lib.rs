extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Config {
    pub address: String,
    pub port: usize,
    pub bearer_token: String,
    pub consumer_key: String,
    pub consumer_secret: String,
    pub access_token: String,
    pub access_token_secret: String,
}
