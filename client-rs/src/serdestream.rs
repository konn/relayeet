use futures::stream::Stream;
use futures::Poll;
use std::fmt::Display;

pub struct SerdeStream<T: Stream>(pub T);

impl<T> Stream for SerdeStream<T>
where
    T: Stream,
    T::Error: Display,
{
    type Item = T::Item;
    type Error = serde_json::Error;

    fn poll(&mut self) -> Poll<Option<T::Item>, Self::Error> {
        self.0.poll().map_err(serde::de::Error::custom)
    }
}
