In this stage, you'll implement the second part of the replication handshake.

### The Handshake Process (Recap)

As a recap, there are three parts to the handshake process:

1. The replica sends a `PING` to the master (Handled in the previous stage)
2. The replica sends `REPLCONF` twice to the master
3. The replica sends `PSYNC` to the master

For this stage, you'll handle the second part of this process.

### The `REPLCONF` Command

The `REPLCONF` command is used to configure a connected replica. After receiving a response to `PING`, the replica sends two `REPLCONF` commands to the master:

1. `REPLCONF listening-port <PORT>`: This tells the master which port the replica is listening on. This value is used for [monitoring and logging](https://github.com/redis/redis/blob/90178712f6eccf1e5b61daa677c5c103114bda3a/src/replication.c#L107-L130), not for replication itself.
2. `REPLCONF capa psync2`: This notifies the master of the replica's capabilities.
   - `capa` stands for "capabilities". It indicates that the next argument is a feature the replica supports.
   - `psync2` signals that the replica supports the PSYNC 2.0 protocol, which is an improved version of the [partial synchronization](https://redis.io/docs/latest/operate/oss_and_stack/management/replication/) feature used to resynchronize a replica with its master.
   - You can safely hardcode `capa psync2` for now.

Both commands should be sent as RESP arrays, so the exact bytes will look something like this:

```
# REPLCONF listening-port <PORT>
*3\r\n$8\r\nREPLCONF\r\n$14\r\nlistening-port\r\n$4\r\n6380\r\n

# REPLCONF capa psync2
*3\r\n$8\r\nREPLCONF\r\n$4\r\ncapa\r\n$6\r\npsync2\r\n
```

For both commands, the master will respond with `+OK\r\n`. That's the string `OK` encoded as a [simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings).

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It will then assert that the replica connects to the master and sends the following:

1. The `PING` command
2. The `REPLCONF` command with `listening-port` and `<PORT>` as the arguments
3. The `REPLCONF` command with `capa psync2` as the arguments

**Notes**

- The response to `REPLCONF` will always be `+OK\r\n`.
