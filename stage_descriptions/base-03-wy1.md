Your server can answer one [PING](https://redis.io/commands/ping). In this
stage, you'll keep the connection usable so the same client can send another
command without reconnecting.

### Handling multiple commands

A Redis server starts to listen for the next command as soon as it's done
responding to the previous one. This allows Redis clients to send multiple
commands using the same connection.

Each `PING` on the connection needs its own `+PONG\r\n` response. Sending a
response must leave the server ready to receive the next command on that
connection.

{{#lang_is_javascript}} If you're using the
[`data`](https://nodejs.org/api/net.html#net_event_data) event, your handler can
receive more input on the same connection. Your existing handler may already
support these sequential requests. Check that it leaves the connection open
after responding. {{/lang_is_javascript}}

{{#lang_is_typescript}} If you're using the
[`data`](https://nodejs.org/api/net.html#net_event_data) event, your handler can
receive more input on the same connection. Your existing handler may already
support these sequential requests. Check that it leaves the connection open
after responding. {{/lang_is_typescript}}

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send multiple `PING` commands using the same connection.

For example, it might send:

```bash
$ echo -e "PING\nPING" | redis-cli
```

The tester will expect to receive a `+PONG\r\n` response for each command sent.

### Notes

- The exact bytes your program will receive won't be just `PING`. You'll receive
  something like this: `*1\r\n$4\r\nPING\r\n`, which is the Redis protocol
  encoding of the `PING` command.
- Just like earlier stages, you can hardcode `+PONG\r\n` as the response for
  this stage. We'll get to parsing client input in later stages.
- The tester will send the `PING` commands using the same connection. We'll get
  to handling multiple connections in later stages.
