Your server can report that it is a replica. In this stage, you'll make it
initiate a connection to its configured master and send `PING`, the first
message in the replication handshake.

### Handshake

The handshake is an exchange of messages that prepares a connection for
replication. For this exchange, the replica acts as a client of the master while
still listening for its own clients.

There are three steps to this handshake:

1. The replica sends a `PING` to the master.
2. The replica sends `REPLCONF` twice to the master.
3. The replica sends `PSYNC` to the master.

We'll learn more about `REPLCONF` and `PSYNC` in later stages. For now, we'll
focus on the first part of the handshake.

When your server starts in replica mode, it must connect to the specified master
host and port, and then send the `PING` command.

The `PING` command must be sent encoded as a
[RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays).
Keep this connection open for the rest of the handshake.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It will then assert that the replica connects to the master and sends the `PING`
command as a RESP array (`*1\r\n$4\r\nPING\r\n`).
