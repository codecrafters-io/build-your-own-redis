In this stage, we'll start implementing support for receiving a replication handshake as a master.

### Handshake (continued from previous stage)

We'll now implement the same handshake we did in the previous stages, but on the master instead of the replica.

As a recap, there are three parts to the handshake:

- The master receives a `PING` from the replica
  - Your Redis server already supports the `PING` command, so there's no additional work to do here
- The master receives `REPLCONF` twice from the replica (**This stage**)
- The master receives `PSYNC` from the replica (Next stage)

In this stage, you'll add support for receiving the `REPLCONF` command from the replica.

You'll receive `REPLCONF` twice from the replica. For the purposes of this challenge, you can safely ignore the arguments for both commands and just
respond with `+OK\r\n` ("OK" encoded as a RESP Simple String).

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It'll then send the following commands:

1. `PING` (expecting `+PONG\r\n` back)
2. `REPLCONF listening-port <PORT>` (expecting `+OK\r\n` back)
3. `REPLCONF capa psync2` (expecting `+OK\r\n` back)
