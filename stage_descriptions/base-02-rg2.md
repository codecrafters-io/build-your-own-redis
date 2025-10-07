In this stage, you'll implement support for the [PING](https://redis.io/commands/ping) command.

### TCP (Transmission Control Protocol)

[TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol) is the underlying protocol used by protocols like HTTP, SSH, and others you're probably familiar with. Redis also uses TCP for communication between its clients and servers.

Don't worry if you're unfamiliar with the TCP protocol, or what Redis clients & servers are. You'll learn more about this in the next stages.

### Redis Commands

Redis clients communicate with Redis servers by sending [commands](https://redis.io/commands/). For each command, a Redis server sends a response back to the client.

For example:

```bash
$ redis-cli SET name Alice
OK
```

Here, the client sends a [`SET`](https://redis.io/docs/latest/commands/set/) command to store the key `name` with the value `Alice`. The server responds with `OK`, confirming that the action was successful.

Both commands and responses are encoded using the [Redis serialization protocol (RESP)](https://redis.io/docs/latest/develop/reference/protocol-spec/). We'll learn more about this in later stages.

### The `PING` Command

[PING](https://redis.io/commands/ping/) is one of the simplest Redis commands. It's used to check whether a Redis server is healthy.

```bash
$ redis-cli PING
PONG
```

The response for the `PING` command is `+PONG\r\n`. This is the string "PONG" encoded as a [RESP simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings).

In this stage, you can ignore the client input and simply hardcode `+PONG\r\n` as a response. We'll get to parsing the client's input in later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send a `PING` command to your server.

```bash
$ redis-cli PING
```

Your server should respond with `+PONG\r\n`, which is `PONG` encoded as a [simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings).

### Notes

- You can ignore the data that the tester sends you for this stage. We'll get to parsing
  client input in later stages. For now, you can just hardcode `+PONG\r\n` as the response.
- You can also ignore handling multiple clients and handling multiple PING commands in this stage—we'll get to that in later stages.
- The exact bytes your program will receive won't just be `PING`. Instead, you'll receive something like this: `*1\r\n$4\r\nPING\r\n`,
  which is the Redis protocol encoding of the `PING` command. We'll learn more about this in later stages.
