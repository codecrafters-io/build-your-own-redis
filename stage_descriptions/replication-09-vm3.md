In this stage, you'll add support for receiving the [`PSYNC`](https://redis.io/commands/psync/) command from the replica.

### Handshake (continued from previous stage)

As a recap, there are three parts to the handshake:

- The master receives a `PING` from the replica (You've already implemented this)
- The master receives `REPLCONF` twice from the replica (You've already implemented this)
- The master receives `PSYNC` from the replica (**This stage**)

After the replica sends `REPLCONF` twice, it'll send a `PSYNC ? -1` command to the master.

- The first argument is `?`
  - This is the replication ID of the master, it is `?` because this is the first time the replica is connecting to the master.
- The second argument is `-1`
  - This is the replication offset, it is `-1` because this is the first time the replica is connecting to the master.

The final command you receive will look something like this:

```
*3\r\n$5\r\nPSYNC\r\n$1\r\n?\r\n$2\r\n-1\r\n
```

(That's `["PSYNC", "?", "-1"]` encoded as a RESP Array)

The master needs to respond with `+FULLRESYNC <REPL_ID> 0\r\n` ("FULLRESYNC <REPL_ID> 0" encoded as a RESP Simple String). Here's what
the response means:

- `FULLRESYNC` means that the master cannot perform incremental replication with the replica, and will thus start a "full" resynchronization.
- `<REPL_ID>` is the replication ID of the master. You've already set this in the "Replication ID & Offset" stage.
  - As an example, you can hardcode `8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb` as the replication ID.
- `0` is the replication offset of the master. You've already set this in the "Replication ID & Offset" stage.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It'll then connect to your TCP server as a replica and execute the following commands:

1. `PING` (expecting `+PONG\r\n` back)
2. `REPLCONF listening-port <PORT>` (expecting `+OK\r\n` back)
3. `REPLCONF capa eof capa psync2` (expecting `+OK\r\n` back)
4. `PSYNC ? -1` (expecting `+FULLRESYNC <REPL_ID> 0\r\n` back)

**Notes**:

- In the response, `<REPL_ID>` needs to be replaced with the replication ID of the master which you've initialized in previous stages.
