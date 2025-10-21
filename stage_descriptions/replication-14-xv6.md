In this stage, you'll implement support for responding to the `REPLCONF GETACK` command as a replica.

### ACKs

Normally, a replica processes propagated commands silently. However, the master needs a way to verify that a replica is "in sync" and hasn't fallen behind. This is done using ACKs (acknowledgements).

Redis masters periodically ask replicas to send ACKs to check how much of the replication stream they’ve processed.

### The `REPLCONF GETACK` command

When the master wants an update, it sends the command:

```bash
REPLCONF GETACK *
```

The exact command received by the replica will look something like this: `*3\r\n$8\r\nreplconf\r\n$6\r\ngetack\r\n$1\r\n*\r\n`. That's `["replconf", "getack", "*"]` encoded as a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays).

The replica receives this command over the replication connection (i.e., the connection used for the replication handshake) and responds with:

```bash
REPLCONF ACK <offset>
```

The offset is the number of bytes of commands processed by the replica. For this stage, you can hardcode the offset to `0`. We'll learn how to track offsets and update them in later stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<HOST> <PORT>"
```

Just like in the previous stages, your replica should complete the handshake with the master and receive an empty RDB file.

The tester will then send `REPLCONF GETACK *` to your replica. 

It will expect to receive `REPLCONF ACK 0` encoded as a RESP array (`*3\r\n$8\r\nREPLCONF\r\n$3\r\nACK\r\n$1\r\n0\r\n`).

### Notes

- After the master-replica handshake is complete, a replica should **only** send responses to `REPLCONF GETACK` commands. All other propagated commands (like `PING`, `SET`, etc.) should be read and processed, but a response should not be sent back to the master.
