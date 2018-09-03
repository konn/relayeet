#[macro_use]
extern crate derive_more;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate router;

extern crate base64;
extern crate hmac;
extern crate iron;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
extern crate subscribe_chan;

use iron::prelude::*;
use router::Router;
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Config {
    pub address: String,
    pub port: usize,
    pub consumer_key: String,
    pub consumer_secret: String,
    pub access_token: String,
    pub access_token_secret: String,
}

impl Config {
    pub fn parse_yaml(path: PathBuf) -> Result<Config> {
        let src = fs::read_to_string(path)?;
        Ok(serde_yaml::from_str(&src)?)
    }
}

pub struct Server {
    config: Config,
    webhook: Webhook,
    bearer: Arc<Mutex<Option<String>>>,
}

pub mod msg;
pub use self::msg::*;

pub mod webhook;
pub use self::webhook::*;

pub mod stream;
pub use self::stream::*;

impl Server {
    fn handle_activity(req: &mut Request) -> IronResult<Response> {
        unimplemented!()
    }

    fn handle_crc(req: &mut Request) -> IronResult<Response> {
        unimplemented!()
    }

    pub fn run(config: Config) -> Result<()> {
        let listen = format!("{}:{}", config.address, config.port);
        let (sender, subscriber) = chan::channel();
        let webhook = Webhook { sender, config };
        // let router = router!(
        //     crc_token: get "/activity?crc_token=:crc_token" => Self::handle_crc,
        //     activity:  post "/activity" =>  webhook,
        // );

        // // Iron::new(router).http(listen)?
        Ok(())
    }
}
