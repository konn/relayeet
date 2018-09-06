# relayeet
Relaying events from Twitter Account Activity API over HTTP, as a stream of events.

## TODOs
### Server-side
* Bearer token issue in parallel
    * Perhaps we must switch to other DB or KVS?

### Client-side
* (Group) DM Notification

## Requirements
### Server-side
* (Of course) Haskell
* [LMBD][LMDB] library

### Client-side
You can write client software on your own.
The default client requires macOS to notify events, with the following cli tool:

* [alerter][alerter]

[LMDB]: http://www.lmdb.tech/doc/
[alerter]: https://github.com/vjeantet/alerter
