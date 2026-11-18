Your replica currently reports offset `0`. In this stage, you'll replace that
fixed value with the number of replication-command bytes processed, so the
master can distinguish an up-to-date replica from one that is behind.

### ACKs and offsets

Each ACK reports a position in the replication stream. Start counting after the
handshake and initial RDB transfer. The handshake messages and snapshot bytes
don't contribute to this offset.

### Offset tracking

A replica keeps its offset updated by tracking the total byte size of every
command received from its master. This includes both write commands (like `SET`,
`DEL`) and non-write commands (like `PING`, `REPLCONF GETACK *`).

After processing a command such as `["SET", "foo", "bar"]`, its full
RESP-encoded byte length contributes to the offset, including the array header,
bulk-string lengths, and CRLF bytes.

An important rule for this process is that the offset should only include
commands processed **before** the current `REPLCONF GETACK *` request.

For example:

- A replica connects, completes the handshake, and the master sends
  `REPLCONF GETACK *`.
  - The replica responds with `REPLCONF ACK 0` since no commands had been
    processed before this request.
- Next, the master sends another `REPLCONF GETACK *`.
  - The replica responds with `REPLCONF ACK 37`, because the previous `REPLCONF`
    command consumed 37 bytes.
- The master then sends a `PING` command.
  - The replica silently processes it, increments its offset by 14, and sends no
    response.
- The next `REPLCONF GETACK *` arrives.
  - The replica responds with `REPLCONF ACK 88`: 37 bytes for each of the two
    earlier `REPLCONF` commands, plus 14 for `PING`.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<HOST> <PORT>"
```

Just like in the previous stages, your replica should complete the handshake
with the master and receive an empty RDB file.

The master will then propagate a series of commands to your replica. These
commands will be interleaved with `REPLCONF GETACK *` commands.

```bash
REPLCONF GETACK *    # expect: REPLCONF ACK 0

PING                 # replica processes silently
REPLCONF GETACK *    # expect: REPLCONF ACK 51
# 51 = 37 (first REPLCONF) + 14 (PING)

SET foo 1             # replica processes silently
SET bar 2             # replica processes silently
REPLCONF GETACK *    # expect: REPLCONF ACK 146
# 146 = 51 + 37 (second REPLCONF) + 29 (SET foo) + 29 (SET bar)
```

Your replica must calculate and return the exact offset at each step in the
`REPLCONF ACK <offset>` response. Your response should also be encoded as a
[RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays).

### Notes

- Although masters don't propagate `PING` commands when received from clients
  (since they aren't "write" commands), they may send `PING` commands to
  replicas to notify replicas that the master is still alive.
