In this stage, you'll implement the first part of the replication handshake.

### The Handshake Process

When a replica connects to a master, it needs to go through a handshake process before receiving updates from the master.

There are three parts to this handshake:

1. The replica sends a `PING` to the master.
2. The replica sends `REPLCONF` twice to the master.
3. The replica sends `PSYNC` to the master.

We'll learn more about `REPLCONF` and `PSYNC` in later stages. For now, we'll focus on the first part of the handshake.

When your server starts in replica mode, it must immediately connect to the specified master host and port, and then send the `PING` command.

The `PING` command must be sent encoded as a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays).

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It will then assert that the replica connects to the master and sends the `PING` command as a RESP array (`*1\r\n$4\r\nPING\r\n`).
