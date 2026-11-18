Your master can forward writes. In this stage, you'll make your replica apply
the writes it receives so clients can read the replicated values from its own
database.

### Command processing

After the initial RDB transfer, commands arrive as RESP arrays on the same
connection. Use the snapshot's advertised byte length to distinguish the binary
file from the commands that follow. Apply each command to the replica's state
without sending an ordinary command response back to the master.

For example, if a master propagates `SET foo 1` to a replica:

- The replica must update its database to set the value of `foo` to `1`.
- Unlike commands from regular clients, the replica does not reply with
  `+OK\r\n`.

### Tests

The tester will spawn a Redis master and execute your program as a replica like
this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

Just like in the previous stages, your replica should complete the handshake
with the master and receive an empty RDB file.

Once the RDB file is received, the master will propagate a series of write
commands to your program:

```bash
SET foo 1 # propagated from master to replica
SET bar 2 # propagated from master to replica
SET baz 3 # propagated from master to replica
```

The tester will then issue `GET` commands to your program to check if the
commands were processed correctly.

```bash
$ redis-cli GET foo # expecting `1` back
$ redis-cli GET bar # expecting `2` back
# ... and so on
```

### Notes

- The propagated commands are sent as RESP arrays. So the command `SET foo 1`
  will be sent as `*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$1\r\n1\r\n`.
- A socket read can contain part of a command or several commands. It can also
  contain the end of the RDB file followed by command bytes. Preserve the RESP
  and RDB boundaries regardless of how the bytes arrive.
