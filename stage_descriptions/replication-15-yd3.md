In this stage, you'll extend your `REPLCONF GETACK` implementation to respond with the number of bytes of commands processed by the replica.

### ACKs (Recap)

As a recap, a master uses ACKs to verify that its replicas are in sync with it and haven't fallen behind. Each ACK contains an offset — the number of bytes of commands processed by the replica.

### Offset tracking

A replica keeps its offset updated by tracking the total byte size of every command received from its master. This includes both write commands (like `SET`, `DEL`) and non-write commands (like `PING`, `REPLCONF GETACK *`).

After processing the received command (e.g., `["SET", "foo", "bar]`), it adds the full RESP array byte length to its running offset.

An important rule for this process is that the offset should only include commands processed **before** the current `REPLCONF GETACK *` request.

For example:

- A replica connects, completes the handshake, and the master sends `REPLCONF GETACK *`.
  - The replica responds with `REPLCONF ACK 0` since no commands had been processed before this request.
- Next, the master sends another `REPLCONF GETACK *`.
  - The replica responds with `REPLCONF ACK 37`, because the previous `REPLCONF` command consumed 37 bytes.
- The master then sends a `PING` command.
  - The replica silently processes it, increments its offset by 14, and sends no response.
- The next `REPLCONF GETACK *` arrives.
  - The replica responds with `REPLCONF ACK 88` — that’s 37 (for the first `REPLCONF`), +37 (for the second `REPLCONF`), +14 (for the `PING`).

Notice that the current `GETACK` request itself is not included in the offset value.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<HOST> <PORT>"
```

Just like in the previous stages, your replica should complete the handshake with the master and receive an empty RDB file.

The master will then propagate a series of commands to your replica. These commands will be interleaved with `REPLCONF GETACK *` commands.

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

Your replica must calculate and return the exact offset at each step in the `REPLCONF ACK <offset>` response. Your response should also be encoded as a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays).

### Notes

- The offset should only include the number of bytes of commands processed **before** receiving the current `REPLCONF GETACK` command.
- Although masters don't propagate `PING` commands when received from clients (since they aren't "write" commands), they may send `PING` commands to replicas to notify replicas that the master is still alive.
