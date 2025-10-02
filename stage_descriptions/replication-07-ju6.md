In this stage, you'll implement the third step of the replication handshake.

### The Handshake Process (Recap)

As a recap, there are three steps to the handshake process:

- The replica sends a `PING` to the master (Handled in an earlier stage)
- The replica sends `REPLCONF` twice to the master (Handled in the previous stage)
- The replica sends `PSYNC` to the master

### The `PSYNC` Command

After receiving a response to the second `REPLCONF`, the replica sends a [`PSYNC`](https://redis.io/commands/psync/) command to the master. 

The `PSYNC` command is used to synchronize the state of the replica with the master. The command format is:

```bash
PSYNC <replication_id> <offset>
```

The command takes two arguments: the master's current replication ID and the replica's current offset.

For the replica's first connection to the master:

- The replication ID will be `?` because the replica doesn't know the master's ID yet.
- The offset will be `-1` since the replica has no data from the master yet.

So the final command sent will be `PSYNC ? -1`, encoded as a RESP array:

```
*3\r\n$5\r\nPSYNC\r\n$1\r\n?\r\n$2\r\n-1\r\n
```

The master will respond with a [simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings) that looks like this:

```
+FULLRESYNC <REPL_ID> 0\r\n
```

You can ignore this response for now. We'll get to handling it in later stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It will then assert that the replica connects to the master and sends:

1. The `PING` command
2. The `REPLCONF` command with `listening-port` and `<PORT>` as arguments
3. The `REPLCONF` command with `capa psync2` as arguments
4. The `PSYNC` command with `? -1` as arguments
