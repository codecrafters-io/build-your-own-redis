In this stage, you’ll extend your `WAIT` implementation to handle the case where replicas are connected and have received write commands.

### `WAIT` with propagated commands

In previous stages, we handled the cases where:
- No replicas were connected, and the master could safely return `0`.
- Replicas were connected, but hadn't received any write commands.

Now, we’ll handle the case where write commands have been sent to replicas. Since replication offsets are no longer `0`, the master needs to check which replicas have successfully processed the latest write command before replying.

To do this, the master must send `REPLCONF GETACK *` to replicas if there are pending write commands since the last `WAIT`. Each replica will reply with its current offset (`REPLCONF ACK <offset>`).

The `WAIT` command should complete when either:

- The required number of replicas has acknowledged the last write command, or
- The timeout expires.

Either way, the master returns the number of replicas that acknowledged the command as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

### Tests

The tester will execute your program as a master like this:

```
./your_program.sh
```

It will then start **multiple** replicas that connect to your server. Each will complete the handshake and expect to receive an empty RDB file.

Next, the tester will connect to your master as a client and send multiple write commands interleaved with `WAIT` commands:

```bash
$ redis-cli SET foo 123
$ redis-cli WAIT 1 500    # (must wait until either 1 replica has processed previous commands or 500ms have passed)

$ redis-cli SET bar 456
$ redis-cli WAIT 2 500    # (must wait until either 2 replicas have processed previous commands or 500ms have passed)
```

### Notes

- The `WAIT` command should always return the number of replicas that have acknowledged the command, even if the timeout expires.
- The returned number of replicas might be lesser than or greater than the expected number of replicas specified in the `WAIT` command.
