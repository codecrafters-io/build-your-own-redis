In this stage you'll implement support for responding to the `REPLCONF GETACK` command as a replica.

### ACKs

<details>
  <summary>Click to expand/collapse</summary>

  Unlike regular commands, when a master forwards commands to a replica via the replication connection, the replica doesn't
  respond to each command. It just silently processes the commands and updates its state.

  Since the master doesn't receive a response for each command, it needs another way to keep track of whether a replica is "in sync".
  That's what ACKs are for.

  ACK is short for "acknowledgement". Redis masters periodically ask replicas to send ACKs.

  Each ACK contains an "offset", which is the number of bytes of commands processed by the replica.

  We'll learn about how this offset is calculated and used in later stages. In this stage, we'll focus on implementing the
  mechanism through which a master asks for an ACK from a replica: the `REPLCONF GETACK` command.
</details>

### The `REPLCONF GETACK` command

<details>
  <summary>Click to expand/collapse</summary>

  When a master requires an ACK from a replica, it sends a `REPLCONF GETACK *` command to the replica. This is sent over
  the replication connection (i.e. the connection that remains after the replication handshake is complete).

  When the replica receives this command, it responds with a `REPLCONF ACK <offset>` response. The offset is the
  number of bytes of commands processed by the replica. It starts at 0 and is incremented for every command processed by the replica.

  In this stage, you'll implement support for receiving the `REPLCONF GETACK *` command and responding with `REPLCONF ACK 0`.

  You can hardcode the offset to 0 for now. We'll implement proper offset tracking in the next stage.

  The exact command received by the replica will look something like this: `*3\r\n$8\r\nreplconf\r\n$6\r\ngetack\r\n$1\r\n*\r\n` (that's
  `["replconf", "getack", "*"]` encoded as a [RESP Array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays)).
</details>

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<HOST> <PORT>"
```

Just like in the previous stages, your replica should complete the handshake with the master and receive an empty RDB file.

The master will then send `REPLCONF GETACK *` to your replica. It'll expect to receive `REPLCONF ACK 0` as a reply.

### Notes

- The response should be encoded as a [RESP Array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays), like
  this: `*3\r\n$8\r\nREPLCONF\r\n$3\r\nACK\r\n$1\r\n0\r\n`.
- We'll implement proper offset tracking in the next stage, for now you can hardcode the offset to 0.
- After the master-replica handshake is complete, a replica should **only** send responses to `REPLCONF GETACK` commands. All
  other propagated commands (like `PING`, `SET` etc.) should be read and processed, but a response should not be sent back to the master.
