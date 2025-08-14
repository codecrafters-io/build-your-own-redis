In this stage, you'll extend your [INFO](https://redis.io/commands/info/) command to run on a replica.

### The `--replicaof` flag

By default, a Redis server assumes the "master" role. When the `--replicaof` flag is passed, the server assumes the "slave" role instead.

Here's an example usage of the `--replicaof` flag:

```
./your_program.sh --port 6380 --replicaof "localhost 6379"
```

In this example, we're starting a Redis server in replica mode. The server itself will listen for connections on port 6380, but it'll
also connect to a master (another Redis server) running on localhost port 6379 and replicate all changes from the master.

We'll learn more about how this replication works in later stages. For now, we'll focus on adding support for the `--replicaof` flag, and
extending the `INFO` command to support returning `role: slave` when the server is a replica.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT> --replicaof "<MASTER_HOST> <MASTER_PORT>"
```

It'll then send the `INFO` command with `replication` as an argument to your server.

```bash
$ redis-cli -p <PORT> info replication
```

Your program should respond with a [Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings) where each line
is a key value pair separated by `:`. The tester will only look for the `role` key, and assert that the value is `slave`.

### Notes

- Your program still needs to pass the previous stage tests, so if `--replicaof` isn't specified, you should default to the `master` role.
- Just like the last stage, you only need to support the `role` key in the response for this stage. We'll add support for the other keys in later stages.
- You don't need to actually connect to the master server specified via `--replicaof` in this stage. We'll get to that in later stages.
