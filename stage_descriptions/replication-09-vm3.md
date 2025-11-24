In this stage, you'll add support for receiving the [`PSYNC`](https://redis.io/commands/psync/) command from the replica.

### Handshake (Recap)

As a recap, the master receives the following for the handshake:

1. A `PING` from the replica
2. `REPLCONF` twice from the replica
3. `PSYNC` from the replica

After the replica sends `REPLCONF` twice, it will send a `PSYNC` command with the arguments `? -1` to the master:

- The replication ID is `?` because the replica doesn't know the master's ID yet.
- The offset is `-1` since the replica has no data from the master yet.

The final command you'll receive will look something like this:

```
*3\r\n$5\r\nPSYNC\r\n$1\r\n?\r\n$2\r\n-1\r\n
```

That's `["PSYNC", "?", "-1"]` encoded as a RESP array.

The master needs to respond with `+FULLRESYNC <REPL_ID> 0\r\n`, which is `FULLRESYNC <REPL_ID> 0` encoded as a simple string. Here's what the response means:

- `FULLRESYNC` means that the master cannot perform an incremental update to the replica, and will start a full resynchronization.
- `<REPL_ID>` is the replication ID of the master.
- `0` is the replication offset of the master.

For example, if your replication ID is `8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb`, you'd respond with:
```bash
+FULLRESYNC 8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb 0\r\n
```

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It will then connect to your TCP server as a replica and send the following commands:

1. `PING` - expecting `+PONG\r\n` back
2. `REPLCONF listening-port <PORT>` - expecting `+OK\r\n` back
3. `REPLCONF capa psync2` - expecting `+OK\r\n` back
4. `PSYNC ? -1` - expecting `+FULLRESYNC <REPL_ID> 0\r\n` back

**Notes**:

- In the response, `<REPL_ID>` needs to be replaced with the replication ID of the master, which you've initialized in previous stages.
