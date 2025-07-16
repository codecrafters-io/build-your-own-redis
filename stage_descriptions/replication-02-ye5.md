In this stage, you'll add support for responding to the [INFO](https://redis.io/commands/info/) command as a master.

The `INFO` command returns information and statistics about a Redis server. In this stage, we'll add support for the `replication` section of the `INFO` command.

### The replication section

When you run the `INFO` command against a Redis server, you'll see something like this:

```
$ redis-cli INFO replication
# Replication
role:master
connected_slaves:0
master_replid:8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb
master_repl_offset:0
second_repl_offset:-1
repl_backlog_active:0
repl_backlog_size:1048576
repl_backlog_first_byte_offset:0
repl_backlog_histlen:
```

The reply to this command is a [Bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings) where each line is a key value pair, separated by ":".

Here are what some of the important fields mean:

- `role`: The role of the server (`master` or `slave`)
- `connected_slaves`: The number of connected replicas
- `master_replid`: The replication ID of the master (we'll get to this in later stages)
- `master_repl_offset`: The replication offset of the master (we'll get to this in later stages)

In this stage, you'll only need to support the `role` key. We'll add support for other keys in later stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It'll then send the `INFO` command with `replication` as an argument.

```bash
$ redis-cli -p <PORT> info replication
```

Your program should respond with a [Bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings) where each line
is a key value pair separated by `:`. The tester will only look for the `role` key, and assert that the value is `master`.

### Notes

- In the response for the `INFO` command, you only need to support the `role` key for this stage. We'll add support for the other keys in later stages.
- The `# Replication` heading in the response is optional, you can ignore it.
- The response to `INFO` needs to be encoded as a [Bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings).
  - An example valid response would be `$11\r\nrole:master\r\n` (the string `role:master` encoded as a [Bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings))
- The `INFO` command can be used without any arguments, in which case it returns all sections available. In this stage, we'll
  always send `replication` as an argument to the `INFO` command, so you only need to support the `replication` section.
