extern crate crossbeam_channel;
use crossbeam_channel as chan;
use std::sync::{Arc, Mutex, Weak};

#[derive(Debug)]
pub struct Sender<T> {
    senders: Arc<Mutex<Vec<Weak<chan::Sender<T>>>>>,
}

#[derive(Debug, Clone)]
pub struct Subscriber<T> {
    senders: Arc<Mutex<Vec<Weak<chan::Sender<T>>>>>,
}

#[derive(Debug)]
pub struct Receiver<T> {
    receiver: chan::Receiver<T>,
    // We don't use _sender; just to make sure it dropped when the Rec'ver dropped
    _sender: Arc<chan::Sender<T>>,
}

pub fn channel<T: Clone>() -> (Sender<T>, Subscriber<T>) {
    let senders = Arc::new(Mutex::new(vec![]));
    (
        Sender {
            senders: senders.clone(),
        },
        Subscriber { senders },
    )
}

impl<T: Clone> Sender<T> {
    pub fn send(&self, t: T) -> () {
        let mut sends = self.senders.lock().unwrap();
        *sends = sends
            .iter()
            .filter_map(|a| a.upgrade())
            .inspect(|s| s.send(t.clone()))
            .map(|a| Arc::downgrade(&a))
            .collect();
    }
}

impl<T> Subscriber<T> {
    pub fn subscribe(&self) -> Receiver<T> {
        let (sender, receiver) = chan::unbounded();
        let _sender = Arc::new(sender);
        let mut sends = self.senders.lock().unwrap();
        (*sends).push(Arc::downgrade(&_sender));
        Receiver { _sender, receiver }
    }
}

impl<T> Receiver<T> {
    pub fn recv(&self) -> Option<T> {
        self.receiver.recv()
    }

    pub fn try_recv(&self) -> Option<T> {
        self.receiver.try_recv()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discards_history() {
        let (s, p) = channel();
        s.send(123);
        let r = p.subscribe();
        assert!(r.try_recv().is_none());
    }

    #[test]
    fn is_dupable() {
        let (s, p) = channel();
        s.send(1);
        let r = p.subscribe();
        s.send(2);
        s.send(3);
        assert_eq!(r.try_recv(), Some(2));
        let r2 = p.subscribe();
        s.send(4);
        assert_eq!(r.try_recv(), Some(3));
        assert_eq!(r2.try_recv(), Some(4));
        assert_eq!(r.try_recv(), Some(4));
        assert_eq!(r2.try_recv(), None);
    }
}
