use iron::prelude::*;
use iron::Handler;
use router::Router;

use chan::*;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

use super::*;

pub struct Webhook {
    pub sender: Sender<Msg>,
    pub config: Config,
}

impl Handler for Webhook {
    fn handle(&self, req: &mut Request) -> IronResult<Response> {
        unimplemented!()
    }
}
