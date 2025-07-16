In this stage, you'll implement part 2 of the handshake that happens when a replica connects to master.

### Handshake (continued from previous stage)

As a recap, there are three parts to the handshake:

- The replica sends a `PING` to the master (Previous stage)
- The replica sends `REPLCONF` twice to the master (**This stage**)
- The replica sends `PSYNC` to the master (Next stage)

After receiving a response to `PING`, the replica then sends 2 [REPLCONF](https://redis.io/commands/replconf/) commands to the master.

The `REPLCONF` command is used to configure replication. Replicas will send this command to the master twice:

- The first time, it'll be sent like this: `REPLCONF listening-port <PORT>`
  - This is the replica notifying the master of the port it's listening on (for [monitoring/logging purposes](https://github.com/redis/redis/blob/90178712f6eccf1e5b61daa677c5c103114bda3a/src/replication.c#L107-L130), not for actual propagation).
- The second time, it'll be sent like this: `REPLCONF capa psync2`
  - This is the replica notifying the master of its capabilities ("capa" is short for "capabilities")
  - You can safely hardcode these capabilities for now, we won't need to use them in this challenge.

These commands should be sent as RESP Arrays, so the exact bytes will look something like this:

```
# REPLCONF listening-port <PORT>
*3\r\n$8\r\nREPLCONF\r\n$14\r\nlistening-port\r\n$4\r\n6380\r\n

# REPLCONF capa psync2
*3\r\n$8\r\nREPLCONF\r\n$4\r\ncapa\r\n$6\r\npsync2\r\n
```

For both commands, the master will respond with `+OK\r\n` ("OK" encoded as a RESP Simple String).

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It'll then assert that the replica connects to the master and:

- **(a)** sends the `PING` command
- **(b)** sends the `REPLCONF` command with `listening-port` and `<PORT>` as arguments
- **(c)** sends the `REPLCONF` command with `capa psync2` as arguments

**Notes**

- The response to `REPLCONF` will always be `+OK\r\n` ("OK" encoded as a RESP Simple String)
