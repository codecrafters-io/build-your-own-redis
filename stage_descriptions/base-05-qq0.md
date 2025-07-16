In this stage, you'll add support for the [ECHO](https://redis.io/commands/echo) command.

`ECHO` is a command like `PING` that's used for testing and debugging. It accepts a single argument and returns it back as a
RESP bulk string.

```bash
$ redis-cli PING # The command you implemented in previous stages
PONG
$ redis-cli ECHO hey # The command you'll implement in this stage
hey
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ECHO` command with an argument to your server:

```bash
$ redis-cli ECHO hey
```

The tester will expect to receive `$3\r\nhey\r\n` as a response (that's the string `hey` encoded as a [RESP bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings).

### Notes

- We suggest that you implement a proper Redis protocol parser in this stage. It'll come in handy in later stages.
- Redis command names are case-insensitive, so `ECHO`, `echo` and `EcHo` are all valid commands.
- The tester will send a random string as an argument to the `ECHO` command, so you won't be able to hardcode the response to pass this stage.
- The exact bytes your program will receive won't be just `ECHO hey`, you'll receive something like this: `*2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n`. That's
  `["ECHO", "hey"]` encoded using the [Redis protocol](https://redis.io/docs/reference/protocol-spec/).
- You can read more about how "commands" are handled in the Redis protocol [here](https://redis.io/docs/reference/protocol-spec/#sending-commands-to-a-redis-server).