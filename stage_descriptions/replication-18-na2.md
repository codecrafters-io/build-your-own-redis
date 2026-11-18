Your master can count replicas at offset zero. In this stage, you'll make `WAIT`
count replicas that have acknowledged the client's preceding writes, waiting for
acknowledgments or the requested timeout.

### `WAIT` with propagated commands

In previous stages, we handled the cases where:

- No replicas were connected, and the master could safely return `0`.
- Replicas were connected, but hadn't received any write commands.

Now, we’ll handle the case where write commands have been sent to replicas.
Since replication offsets are no longer `0`, the master needs to check which
replicas have successfully processed the latest write command before replying.

To do this, the master must send `REPLCONF GETACK *` to replicas if there are
pending write commands since the last `WAIT`. Each replica will reply with its
current offset (`REPLCONF ACK <offset>`).

The `WAIT` command should complete when either:

- The required number of replicas has acknowledged all previous write commands,
  or
- The timeout expires.

Either way, return the number of qualifying replicas as a
[RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).
A replica qualifies when its acknowledged offset reaches the end of the
preceding writes from this client connection. Count each replica once. A
connection alone doesn't show that it has processed those writes.

### Tests

The tester will execute your program as a master like this:

```
./your_program.sh
```

It will then start **multiple** replicas that connect to your server. Each will
complete the handshake and expect to receive an empty RDB file.

Next, the tester will connect to your master as a client and send multiple write
commands interleaved with `WAIT` commands:

```text
# Commands within one redis-cli session, using the same connection:
SET foo 123
WAIT 1 500    # wait for 1 replica or 500 ms

SET bar 456
WAIT 2 500    # wait for 2 replicas or 500 ms
```

### Notes

- The returned number of replicas might be less than or greater than the
  expected number of replicas specified in the `WAIT` command.
