In this stage, you'll implement part 1 of the handshake that happens when a replica connects to master.

### Handshake

When a replica connects to a master, it needs to go through a handshake process before receiving updates from the master.

There are three parts to this handshake:

- The replica sends a `PING` to the master (**This stage**)
- The replica sends `REPLCONF` twice to the master (Next stages)
- The replica sends `PSYNC` to the master (Next stages)

We'll learn more about `REPLCONF` and `PSYNC` in later stages. For now, we'll focus on the first part of the handshake: sending `PING` to the master.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It'll then assert that the replica connects to the master and sends the `PING` command.

### Notes

- The `PING` command should be sent as a RESP Array, like this : `*1\r\n$4\r\nPING\r\n`
