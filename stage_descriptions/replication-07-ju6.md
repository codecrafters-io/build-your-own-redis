In this stage, you'll implement part 3 of the handshake that happens when a replica connects to master.

### Handshake (continued from previous stage)

As a recap, there are three parts to the handshake:

- The replica sends a `PING` to the master (Previous stages)
- The replica sends `REPLCONF` twice to the master (Previous stages)
- The replica sends `PSYNC` to the master (**This stage**)

After receiving a response to the second `REPLCONF`, the replica then sends a [PSYNC](https://redis.io/commands/psync/) command to the master.

The `PSYNC` command is used to synchronize the state of the replica with the master. The replica will send this command to the master with two arguments:

- The first argument is the replication ID of the master
  - Since this is the first time the replica is connecting to the master, the replication ID will be `?` (a question mark)
- The second argument is the offset of the master
  - Since this is the first time the replica is connecting to the master, the offset will be `-1`

So the final command sent will be `PSYNC ? -1`.

This should be sent as a RESP Array, so the exact bytes will look something like this:

```
*3\r\n$5\r\nPSYNC\r\n$1\r\n?\r\n$2\r\n-1\r\n
```

The master will respond with a [Simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings) that looks like this:

```
+FULLRESYNC <REPL_ID> 0\r\n
```

You can ignore the response for now, we'll get to handling it in later stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It'll then assert that the replica connects to the master and:

- **(a)** sends `PING` command
- **(b)** sends `REPLCONF listening-port <PORT>`
- **(c)** sends `REPLCONF capa eof capa psync2`
- **(d)** sends `PSYNC ? -1`
