use hyper::{Body, Response, StatusCode};

use gotham::handler::{Handler, HandlerFuture};
use gotham::http::response::create_response;
use gotham::router::builder::*;
use gotham::router::Router;
use gotham::state::{FromState, State};
use hyper::header::*;
use hyper::Method;
use mime;

use chan::*;
use std::fmt;
use std::fs;
use std::io;
use std::io::Read;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

use super::*;

pub struct Webhook {
    pub sender: Sender<Msg>,
    pub config: WebhookConfig,
}

struct SenderHandler {
    pub sender: Sender<Msg>,
    pub consumer_secret: String,
}

fn decode_sig(str: String) -> Option<Vec<u8>> {
    if !str.starts_with("sha256=") {
        None
    } else if let Ok(code) = base64::decode(&str[7..]) {
        Some(code)
    } else {
        None
    }
}

impl SenderHandler {
    fn verify_request(&self, state: &State) -> bool {
        let headers = HeaderMap::<HeaderValue>::borrow_from(state);
        if let Some(sig) = headers
            .get("x-twitter-webhooks-signature")
            .and_then(decode_sig)
        {
            state.borrow::<Body>().concat2().then(|full| {
                if let Some(bdy) = full {
                    let mut mac = HmacSha256::new_varkey(self.consumer_secret.as_bytes()).unwrap();
                    mac.input(bdy.as_bytes());
                    return mac.verify(&sig[0..]).is_ok();
                }
            })
        }
        false
    }
}

impl Handler for SenderHandler {
    fn handle(self, mut state: State) -> Box<HandlerFuture> {
        let rsp = if let Method::Post = state.take() {
            if self.verify_request(&state) {
                let src = state.take::<Body>().collect().wait();
                if let Ok(msg) = serde_json::from_slice(&src) {
                    self.sender.send(Msg(msg));
                    create_response(&state, StatusCode::OK, "OK", mime::TEXT_PLAIN)
                } else {
                    create_response(&state, StatusCode::BAD_REQUEST, "Invalid JSON")
                }
            } else {
                create_response(&state, StatusCode::UNAUTHORIZED, "Unahotorized")
            }
        } else {
            create_response(&state, StatusCode::METHOD_NOT_ALLOWED, "Method not allowed")
        };
        (state, rsp)
    }
}

#[derive(Deserialize, StateData)]
struct CRCToken {
    crc_token: String,
}

impl Webhook {
    fn router(self, handler: SenderHandler, secret: String) -> Router {
        let handle_crc = move |mut state: State| -> (State, Response<Body>) {
            let CRCToken { crc_token: crc } = CRCToken::take_from(&mut state);
            let result = b64_hmac_sha256(secret.as_bytes(), crc.as_bytes());
            let rsp = CRCResult {
                response_token: format!("sha256={}", result),
            };
            create_response(
                &state,
                StatusCode::OK,
                serde_json::to_string(&rsp).unwrap(),
                mime::APPLICATION_JSON,
            )
        };
        build_simple_router(|route| {
            route.get("/activity").to(handle_crc);
            route.post("/activity").to(handler);
        })
    }
    pub fn run(self) -> () {
        let config = self.config;
        let secret = config.consumer_secret;
        let secret_dup = secret.clone();
        let addr = config.address;
        let port = config.port;
        let handler = SenderHandler {
            sender: self.sender,
            consumer_secret: secret,
        };
        let bind = format!("{}:{}", addr, port);
        gotham::start(bind, self.router(handler, secret_dup))
    }
}
