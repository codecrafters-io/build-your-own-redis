In this stage, you’ll extend your `WAIT` implementation to handle the case where replicas are connected and have received write commands.

### `WAIT` with propagated commands

In previous stages, we handled the cases where:
- No replicas were connected, and the master could safely return `0`.
- Replicas were connected, but hadn't received any write commands.

Now, we’ll handle the case where write commands have been sent to replicas. Since replication offsets are no longer `0`, the master needs to check which replicas have successfully processed the latest write command before replying.

To do this, the master must explicitly send a `REPLCONF GETACK *` command to replicas. Each replica will reply with its current offset (`REPLCONF ACK <offset>`). Using these offsets, the master can determine how many replicas have processed the last write.

The `WAIT` command should complete when either:

- The required number of replicas has acknowledged the last write, or
- The timeout expires.

Either way, the master returns the number of replicas that acknowledged the command.

Here’s an example:

```bash
$ redis-cli SET foo 123
"OK"

$ redis-cli WAIT 1 500
(integer) 1   # returned as soon as 1 replica confirmed the write

$ redis-cli SET bar 456
"OK"

$ redis-cli WAIT 2 500
(integer) 2   # returned when 2 replicas confirmed within 500ms
```

### Tests

The tester will execute your program as a master like this:

```
./your_program.sh
```

It'll then start **multiple** replicas that connect to your server. Each will complete the handshake and expect to receive an empty RDB file.

The tester will then connect to your master as a Redis client (not one of the replicas) and send multiple write commands interleaved
with `WAIT` commands:

```bash
$ redis-cli SET foo 123
$ redis-cli WAIT 1 500 # (must wait until either 1 replica has processed previous commands or 500ms have passed)

$ redis-cli SET bar 456
$ redis-cli WAIT 2 500 # (must wait until either 2 replicas have processed previous commands or 500ms have passed)
```

### Notes

- The `WAIT` command should return when either (a) the specified number of replicas have acknowledged the command, or (b) the timeout expires.
- The `WAIT` command should always return the number of replicas that have acknowledged the command, even if the timeout expires.
- The returned number of replicas might be lesser than or greater than the expected number of replicas specified in the `WAIT` command.
