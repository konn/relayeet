use futures::stream::Stream;
use futures::Poll;

pub struct SerdeStream<T: Stream>(pub T);

impl<T> Stream for SerdeStream<T>
where
    T: Stream,
{
    type Item = T::Item;
    type Error = serde_json::Error;

    fn poll(&mut self) -> Poll<Option<T::Item>, Self::Error> {
        self.0
            .poll()
            .map_err(|_| serde::de::Error::custom("Unknown Readlines error"))
    }
}
