In this stage, you'll implement support for the [PING](https://redis.io/commands/ping) command.

Redis clients communicate with Redis servers by sending "[commands](https://redis.io/commands/)". For each command, a Redis server sends a response back to the client.
Commands and responses are both encoded using the [Redis protocol](https://redis.io/topics/protocol) (we'll learn more about this in later stages).

[PING](https://redis.io/commands/ping/) is one of the simplest Redis commands. It's used to check whether a Redis server is healthy.

The response for the `PING` command is `+PONG\r\n`. This is the string "PONG" encoded using the [Redis protocol](https://redis.io/docs/reference/protocol-spec/).

In this stage, we'll cut corners by ignoring client input and hardcoding `+PONG\r\n` as a response. We'll learn to parse client input in later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send a `PING` command to your server and expect a `+PONG\r\n` response.

```bash
$ redis-cli PING
```

Your server should respond with `+PONG\r\n`, which is "PONG" encoded as a [RESP simple string](https://redis.io/docs/reference/protocol-spec/#resp-simple-strings).

### Notes

- You can ignore the data that the tester sends you for this stage. We'll get to parsing
client input in later stages. For now, you can just hardcode `+PONG\r\n` as the response.
- You can also ignore handling multiple clients and handling multiple PING commands in the stage, we'll get to that in later stages.
- The exact bytes your program will receive won't be just `PING`, you'll receive something like this: `*1\r\n$4\r\nPING\r\n`,
which is the Redis protocol encoding of the `PING` command. We'll learn more about this in later stages.