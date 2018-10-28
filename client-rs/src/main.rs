extern crate actix_web;
extern crate futures;
extern crate openssl;
extern crate relayeet_client;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
extern crate tokio_serde_json;

use actix_web::{actix, client, HttpMessage};
use futures::future::Future;
use futures::stream::Stream;
use relayeet_client::*;
use serde_json::Value;
use std::env;
use std::fs;
use tokio_serde_json::ReadJson;

fn main() {
    let args: Vec<_> = env::args().collect();
    let src = fs::read_to_string(&args[1]).unwrap();
    let c: ClientConfig = serde_yaml::from_str(&src).unwrap();
    let hauth = format!("Bearer {}", c.bearer);

    actix::run(|| {
        client::get(c.url)
            .header("Authorization", hauth)
            .finish()
            .unwrap()
            .send()
            .map_err(|_| ())
            .and_then(|rsp| {
                ReadJson::<_, Value>::new(SerdeStream(rsp.payload()))
                    .for_each(|j| {
                        println!("{:?}", j);
                        Ok(())
                    }).map_err(|_| ())
            }).map_err(|_| ())
    });
}
