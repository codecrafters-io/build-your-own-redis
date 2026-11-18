Your server can run on a chosen port. In this stage, you'll implement
[INFO](https://redis.io/commands/info/) to report the master role to clients.

### The `INFO` command

The `INFO` command returns information and statistics about a running Redis
server. For example, a client can get information about a server like this:

```bash
$ redis-cli INFO
# Server
redis_version:7.2.4
...
# Clients
connected_clients:1
...
# Memory
used_memory:859944
...
# Replication
role:master
...
```

The server then responds with a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings),
where each line is a key-value pair separated by a colon (`:`). The string can
also contain section header lines (starting with `#`) and blank lines.

The `INFO` command also accepts an optional parameter to specify which section
of information to display, such as `server`, `memory`, or `replication`. For
this stage, we'll only focus on the `replication` section.

### The `replication` section

When you run the `INFO` command with the `replication` argument, the server
returns only the details concerning its replication setup:

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

Here are what some of the important fields mean:

- `role`: The role of the server (either `master` or `slave`).
- `connected_slaves`: The number of connected replica servers.
- `master_replid`: The replication ID of the master.
- `master_repl_offset`: The replication offset of the master.

Only the `role` field is required now. A minimal response is
`$11\r\nrole:master\r\n`, a bulk string containing the eleven bytes of
`role:master`.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It will then send the `INFO` command with `replication` as an argument.

```bash
$ redis-cli -p <PORT> info replication
```

Your server should respond with a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings)
where each line is a key value pair separated by a colon (`:`). The tester will
only look for the `role` key and assert that the value is `master`.

### Notes

- The `# Replication` heading in the response is optional, and you can ignore
  it.
- If you include multiple fields, separate the lines inside the bulk string with
  CRLF and include those bytes in its length.
