#[macro_use]
extern crate derive_more;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate gotham_derive;

extern crate base64;
extern crate gotham;
extern crate hmac;
extern crate hyper;
extern crate mime;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
extern crate sha2;
extern crate subscribe_chan;

use std::fs;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use subscribe_chan as chan;

#[derive(Debug, From)]
pub enum Error {
    IOError(io::Error),
    SerdeYamlError(serde_yaml::Error),
    InvalidPath(PathBuf),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WebhookConfig {
    pub address: String,
    pub port: usize,
    pub consumer_key: String,
    pub consumer_secret: String,
    pub access_token: String,
    pub access_token_secret: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StreamConfig {
    pub address: String,
    pub port: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Config {
    pub webhook: WebhookConfig,
    pub stream: StreamConfig,
}

impl Config {
    pub fn parse_yaml(path: PathBuf) -> Result<Config> {
        let src = fs::read_to_string(path)?;
        Ok(serde_yaml::from_str(&src)?)
    }
}

pub mod msg;
pub use self::msg::*;

pub mod webhook;
pub use self::webhook::*;

use hmac::{Hmac, Mac};
use sha2::Sha256;

type HmacSha256 = Hmac<Sha256>;

pub struct Server {
    config: Config,
    webhook: Webhook,
    chan_token: chan::Token<Msg>,
}

#[derive(Serialize, Debug)]
struct CRCResult {
    response_token: String,
}

fn b64_hmac_sha256(key: &[u8], val: &[u8]) -> String {
    use base64;

    let mut hmac = HmacSha256::new_varkey(key).unwrap();
    hmac.input(val);
    base64::encode(hmac.result().code().as_slice())
}

impl Server {
    pub fn new(config: Config) -> Self {
        let (sender, chan_token) = chan::channel();
        let webhook = Webhook {
            sender,
            config: config.webhook.clone(),
        };
        Server {
            config,
            webhook,
            chan_token,
        }
    }

    pub fn run(self) -> Result<()> {
        let webhook = self.webhook;

        let webhook_srv = thread::spawn(move || webhook.run());
        webhook_srv.join().unwrap();
        Ok(())
    }
}
