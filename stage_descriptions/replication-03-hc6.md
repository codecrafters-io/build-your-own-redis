Your [INFO](https://redis.io/commands/info/) response currently reports
`master`. In this stage, you'll let startup configuration select the replica
role and report it as `slave`, the literal Redis uses in this response.

### The `--replicaof` flag

By default, your Redis server assumes the master role. When you pass the
`--replicaof` flag, the server assumes the replica role instead.

For example:

```
./your_program.sh --port 6380 --replicaof "localhost 6379"
```

Here, the server listens on port `6380`. The quoted argument after `--replicaof`
contains the master's hostname and port separated by a space. Those identify the
server it will replicate from in later stages. No connection to that master is
required yet.

For this stage, your primary task is to update the `INFO replication` command
handler to check the server's configuration:

- If the user does not include the `--replicaof` flag, respond with
  `role:master`.
- If the user includes the `--replicaof` flag, respond with `role:slave`.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It will then send the `INFO` command with a `replication` argument to your
server.

```bash
$ redis-cli -p <PORT> INFO replication
```

Your program should respond with a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings)
where each line is a key-value pair separated by a colon (`:`). The tester will
only look for the `role` key and assert that the value is `slave`.
