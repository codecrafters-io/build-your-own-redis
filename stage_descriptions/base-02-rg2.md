Your server can accept a connection. In this stage, you'll make it respond to
`PING` so a client can check that the server is ready to handle commands.

### What is `PING`?

Redis clients talk to Redis servers by sending **commands** and receiving
**responses**.

For example:

```bash
redis> PING                  # client sends a PING command
"PONG"                       # server replies with PONG

redis> SET name Alice        # store the value "Alice" under the key "name"
OK                           # server confirms success
```

`PING` checks whether a Redis server is alive and responding. The example above
shows a client's display. You don't need to implement `SET` yet.

The response bytes are `+PONG\r\n`. This is a **simple string** in RESP, the
Redis Serialization Protocol. The `+` identifies the response type. The ending
`\r\n` represents two bytes, carriage return followed by line feed, also called
CRLF. Send those bytes over the connection, rather than printing the backslash
characters or writing the response to your program's terminal.

For this stage, you can hardcode `+PONG\r\n` as the response to received input.
Parsing the incoming command comes in later stages.

### Tests

The tester starts your server with `./your_program.sh`, sends a `PING` command,
and checks for the `+PONG\r\n` response.
