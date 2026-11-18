Your server handles repeated commands on one connection. In this stage, you'll
let multiple clients keep connections open and receive responses without waiting
for another client to disconnect.

### Handling multiple clients

These overlapping connections are **concurrent**. Each client's `PING` needs a
`+PONG\r\n` response on that client's connection, even while another connection
is idle.

{{#lang_is_javascript}} The runtime's
[event loop](https://en.wikipedia.org/wiki/Event_loop) can handle events from
several connections. With
[event-driven networking](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop),
your existing server may already support this. Check that handling one
connection doesn't block the others. {{/lang_is_javascript}}

{{#lang_is_typescript}} The runtime's
[event loop](https://en.wikipedia.org/wiki/Event_loop) can handle events from
several connections. With
[event-driven networking](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop),
your existing server may already support this. Check that handling one
connection doesn't block the others. {{/lang_is_typescript}}

{{^lang_is_javascript}} {{^lang_is_typescript}} Threads and an
[event loop](https://en.wikipedia.org/wiki/Event_loop) are possible approaches.
An event loop waits for activity across connections and handles each ready
connection. You can use your language's networking facilities to choose how to
support overlapping clients. {{/lang_is_typescript}} {{/lang_is_javascript}}

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll send `PING` commands across several connections that remain open at the
same time. For example, two clients can each send:

```bash
# These two will be sent concurrently so that we can test your server's ability to handle concurrent clients.
$ redis-cli PING
$ redis-cli PING
```

The tester expects a separate `+PONG\r\n` response for each command, on the
connection that sent it.

### Notes

- Since the tester client _only_ sends the `PING` command at the moment, it's
  okay to ignore what the client sends and hardcode a response. We'll get to
  parsing client input in later stages.
