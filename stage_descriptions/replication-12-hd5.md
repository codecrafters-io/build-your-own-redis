In this stage, you'll extend your implementation of the master to support propagating commands to multiple replicas.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It'll then start **multiple** replicas that connect to your server and execute the following commands:

1. `PING` (expecting `+PONG\r\n` back)
2. `REPLCONF listening-port <PORT>` (expecting `+OK\r\n` back)
3. `REPLCONF capa psync2` (expecting `+OK\r\n` back)
4. `PSYNC ? -1` (expecting `+FULLRESYNC <REPL_ID> 0\r\n` back)

Each replica will expect to receive an RDB file from the master after the handshake is complete.

It'll then send `SET` commands to the master from a client (a separate Redis client, not the replicas).

```bash
$ redis-cli SET foo 1
$ redis-cli SET bar 2
$ redis-cli SET baz 3
```

It'll then assert that each replica received those commands, in order.
