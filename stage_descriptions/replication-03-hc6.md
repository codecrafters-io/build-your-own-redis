In this stage, you'll extend the [INFO](https://redis.io/commands/info/) command to reflect a server's role as a replica.

### The `--replicaof` Flag

By default, your Redis server assumes the master role. When you pass the `--replicaof` flag, the server assumes the slave role instead.

For example:

```
./your_program.sh --port 6380 --replicaof "localhost 6379"
```

Here, we use the `--replicaof` flag to start a Redis server as a replica. The server will listen for connections on port `6380`, but it will also connect to a master (another Redis server) running on `localhost:6379` and replicate all its changes.

We'll learn more about how this replication works in later stages. 

For this stage, your primary task is to update the `INFO replication` command handler to check the server's runtime configuration:

- If the user does not include the `--replicaof` flag, respond with `role:master`.
- If the user includes the `--replicaof` flag, respond with `role:slave`.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It will then send the `INFO` command with a `replication` argument to your server.

```bash
$ redis-cli -p <PORT> info replication
```

Your program should respond with a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings) where each line
is a key-value pair separated by a colon (`:`). The tester will only look for the `role` key, and assert that the value is `slave`.

### Notes

- Your program still needs to pass the previous stage tests, so if `--replicaof` isn't specified, you should default to the `master` role.
- Just like the last stage, you only need to support the `role` key in the response for this stage. We'll add support for the other keys in later stages.
- You don't need to actually connect to the master server specified via `--replicaof` in this stage. We'll get to that in later stages.
