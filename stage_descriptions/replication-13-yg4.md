In this stage, you'll implement the processing of propagated commands as a replica.

### Command Processing

After the replica receives a command from the master, it processes it and applies it to its own state. This will work exactly like a regular command sent by a client. The key difference is that the replica **must not send a response** back to the master.

For example, if a master propagates `SET foo 1` to a replica:

- The replica must update its database to set the value of `foo` to `1`.
- Unlike commands from regular clients, the replica does not reply with `+OK\r\n`.

### Tests

The tester will spawn a Redis master and execute your program as a replica like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

Just like in the previous stages, your replica should complete the handshake with the master and receive an empty RDB file.

Once the RDB file is received, the master will propagate a series of write commands to your program:

```bash
SET foo 1 # propagated from master to replica
SET bar 2 # propagated from master to replica
SET baz 3 # propagated from master to replica
```

The tester will then issue `GET` commands to your program to check if the commands were processed correctly.

```bash
$ redis-cli GET foo # expecting `1` back
$ redis-cli GET bar # expecting `2` back
# ... and so on
```

### Notes

- The propagated commands are sent as RESP arrays. So the command `SET foo 1` will be sent as `*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$1\r\n1\r\n`.
- It is **not** guaranteed that propagated commands will be sent one at a time. One TCP segment might contain bytes for multiple commands.
