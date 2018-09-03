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

struct Stream {
    events: Receiver<Msg>,
}
