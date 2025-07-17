In this stage, you'll extend your `REPLCONF GETACK` implementation to respond with the number of bytes of commands processed by the replica.

### Offset tracking

<details>
  <summary>Click to expand/collapse</summary>
  As we saw in previous stages, when a replica receives a command from the master, it processes it and updates its state. In addition to processing
  commands, the replica also keeps a running count of the number of bytes of commands it has processed.

  This count is called the "offset". When a master sends a `REPLCONF GETACK` command to a replica, the replica is expected to respond with
  `REPLCONF ACK <offset>`. The returned `<offset>` should only include the number of bytes of commands processed **before** receiving the `REPLCONF GETACK` command.

  As an example:

  - Let's say a replica connects to a master and completes the handshake.
  - The master then sends a `REPLCONF GETACK *` command.
      - The replica should respond with `REPLCONF ACK 0`.
      - The returned offset is 0 since no commands have been processed yet (before receiving the `REPLCONF GETACK` command)
  - The master then sends `REPLCONF GETACK *` again.
      - The replica should respond with `REPLCONF ACK 37`.
      - The returned offset is 37 since the first `REPLCONF GETACK` command was processed, and it was 37 bytes long.
      - The RESP encoding for the `REPLCONF GETACK` command looks like this: ``*3\r\n$8\r\nreplconf\r\n$6\r\ngetack\r\n$1\r\n*\r\n` (that's 37 bytes long)
  - The master then sends a `PING` command to the replica (masters do this periodically to notify replicas that the master is still alive).
      - The replica must silently process the `PING` command and update its offset. It should not send a response back to the master.
  - The master then sends `REPLCONF GETACK *` again (this is the third REPLCONF GETACK command received by the replica)
      - The replica should respond with `REPLCONF ACK 88`.
      - The returned offset is 88 (37 + 37 + 14)
          - 37 for the first `REPLCONF GETACK` command
          - 37 for the second `REPLCONF GETACK` command
          - 14 for the `PING` command
      - Note that the third `REPLCONF GETACK` command is not included in the offset, since the value should
      only include the number of bytes of commands processed **before** receiving the current `REPLCONF GETACK` command.
  - ... and so on

</details>

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<HOST> <PORT>"
```

Just like in the previous stages, your replica should complete the handshake with the master and receive an empty RDB file.

The master will then propagate a series of commands to your replica. These commands will be interleaved with `REPLCONF GETACK *` commands.

```bash
REPLCONF getack * # expecting REPLCONF ACK 0, since 0 bytes have been processed

ping # master sending a ping command to notify the replica that it's still alive
REPLCONF getack * # expecting REPLCONF ACK 51 (37 for the first REPLCONF command + 14 for the ping command)

set foo 1 # propagated from master to replica
set bar 2 # propagated from master to replica
REPLCONF getack * # expecting REPLCONF ACK 109 (51 + 29 for the first set command + 29 for the second set command)
```

### Notes

- The offset should only include the number of bytes of commands processed **before** receiving the current `REPLCONF GETACK` command.
- Although masters don't propagate `PING` commands when received from clients (since they aren't "write" commands),
  they may send `PING` commands to replicas to notify replicas that the master is still alive.
- Replicas should update their offset to account for **all** commands propagated from the master, including `PING` and `REPLCONF` itself.
- The response should be encoded as a [RESP Array](https://redis.io/docs/reference/protocol-spec/#arrays), like
  this: `*3\r\n$8\r\nREPLCONF\r\n$3\r\nACK\r\n$3\r\n154\r\n`.
