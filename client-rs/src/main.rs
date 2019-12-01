extern crate actix_web;
extern crate futures;
extern crate openssl;
extern crate relayeet_client;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
extern crate tokio_serde_json;

use actix_web::{actix, actix::Actor, client, client::ClientConnector, HttpMessage};
use futures::future::Future;
use futures::stream::Stream;
use openssl::ssl::{SslConnector, SslMethod};
use relayeet_client::*;
use serde_json::Value;
use std::env;
use std::fs;
use std::time::Duration;
use tokio_serde_json::ReadJson;

fn main() {
    let args: Vec<_> = env::args().collect();
    let src = fs::read_to_string(&args[1]).unwrap();
    let c: ClientConfig = serde_yaml::from_str(&src).unwrap();
    let hauth = format!("Bearer {}", c.bearer);

    actix::run(|| {
        let ssl_conn = SslConnector::builder(SslMethod::tls()).unwrap().build();
        let conn = ClientConnector::with_connector(ssl_conn);
        let conn = conn.conn_keep_alive(Duration::from_secs(30)).start();
        client::get(c.url)
            .with_connector(conn)
            .header("Authorization", hauth)
            .timeout(Duration::from_secs(30))
            .finish()
            .unwrap()
            .send()
            .map_err(|_| ())
            .and_then(|rsp| {
                rsp.payload()
                    .for_each(|j| {
                        println!("{:?}", j);
                        Ok(())
                    }).map_err(|_| ())
            }).map_err(|_| ())
            .and_then(|_| {
                println!("Finished!");
                Ok(())
            })
    });
}
