In this stage, you'll respond to multiple
[PING](https://redis.io/commands/ping) commands sent by the same connection.

A Redis server starts to listen for the next command as soon as it's done responding to the previous one. This allows
Redis clients to send multiple commands using the same connection.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send multiple PING commands using the same connection. For example, it might send:

```bash
$ echo -e "PING\nPING" | redis-cli
```

The tester will expect to receive multiple `+PONG\r\n` responses (one for each command sent).

{{#lang_is_javascript}}
In most languages, you'd need to run a loop that reads input from a connection and sends a
response back. In JavaScript however, if you're listening to the
[`data`](https://nodejs.org/api/net.html#net_event_data) event, this should be automatically handled for you. **It
is very likely that the code you had for the previous stage will pass this stage without any changes!**
{{/lang_is_javascript}}

{{^lang_is_javascript}}
You'll need to run a loop that reads input from a connection and sends a
response back.
{{/lang_is_javascript}}

### Notes

- Just like the previous stage, you can hardcode `+PONG\r\n` as the response for this stage. We'll get to parsing
 client input in later stages.
- The PING commands will be sent using the same connection. We'll get to handling multiple connections in later stages.