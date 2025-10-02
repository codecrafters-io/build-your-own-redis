In this stage, we'll start implementing support for receiving a replication handshake as a master.

### The Handshake Process (Recap)

Up until now, we've been implementing the handshake from the replica's perspective. Now we'll implement the same handshake on the master side.

As a recap, the master receives the following from the replica during the handshake process:

1. A `PING` command
2. Two `REPLCONF` commands
3. A `PSYNC` command

Your Redis server already supports the `PING` command, so there's no additional work to do for the first step.

In this stage, you'll add support for receiving the two `REPLCONF` commands as a master.

For the purposes of this challenge, you can safely ignore the arguments for both commands and simply respond with `+OK\r\n`. That's the string `OK` encoded as a [simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings)

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It will then send the following commands:

1. `PING` — expecting `+PONG\r\n` as the response
2. `REPLCONF listening-port <PORT>` — expecting `+OK\r\n`
3. `REPLCONF capa psync2` — expecting `+OK\r\n` 
