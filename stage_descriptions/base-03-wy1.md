In this stage, you'll respond to multiple [PING](https://redis.io/commands/ping) commands sent by the same connection.

### Handling Multiple Commands

A Redis server starts to listen for the next command as soon as it's done responding to the previous one. This allows
Redis clients to send multiple commands using the same connection.

For this stage, your server should run a loop that continuously reads commands and sends responses back through the same connection. Since we're only dealing with the `PING` command for now, your program can run a loop that reads input and responds with `+PONG\r\n`.

{{#lang_is_javascript}}
In most languages, you'd need to write specific code to run this loop on your server. In JavaScript, however, if you're listening to the [`data`](https://nodejs.org/api/net.html#net_event_data) event, this should be automatically handled for you. **It is very likely that the code you used in the previous stage will pass this stage without any changes!**
{{/lang_is_javascript}}

{{#lang_is_typescript}}
In most languages, you'd need to write specific code to run this loop on your server. In JavaScript, however, if you're listening to the [`data`](https://nodejs.org/api/net.html#net_event_data) event, this should be automatically handled for you. **It is very likely that the code you used in the previous stage will pass this stage without any changes!**
{{/lang_is_typescript}}


### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send multiple `PING` commands using the same connection. For example, it might send:

```bash
$ echo -e "PING\nPING" | redis-cli
```

The tester will expect to receive a `+PONG\r\n` response for each command sent.

### Notes

- The exact bytes your program will receive won't be just `PING`, you'll receive something like this: `*1\r\n$4\r\nPING\r\n`, which is the Redis protocol encoding of the `PING` command.
- Just like the previous stage, you can hardcode `+PONG\r\n` as the response for this stage. We'll get to parsing client input in later stages.
- The tester will send the `PING` commands using the same connection. We'll get to handling multiple connections in later stages.
