In this stage, you'll add support for starting the Redis server on a custom port.

### Leader-Follower Replication

The [leader-follower replication](https://redis.io/docs/latest/operate/oss_and_stack/management/replication/) is a pattern where one server (the "master") handles all write operations, and one or more servers (the "replicas") maintain copies of the master's data. When the master changes data, it automatically copies those changes to the replicas. This system provides data redundancy and improves read performance.

### Custom Port Support

Since replication requires running multiple Redis servers simultaneously, each instance needs its own port. This means a Redis server must be able to start on a port other than the default `6379`.

The `--port` flag passes the port number to the Redis server:

```bash
./your_program.sh --port <port_number>
```

The server then parses this argument and starts a TCP server on the specified port.

If you don’t provide a `--port` flag, the Redis server defaults to port `6379`.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port 6380
```

It'll then try to connect to your TCP server on the specified port number. If the connection succeeds, you'll pass this stage.

### Notes

- The tester will pass a random port number to your program, so you can't hardcode the port number from the example above.
- If your repository was created before 5th Oct 2023, it's possible that your `./your_program.sh` script
might not be passing arguments on to your program. You'll need to edit `./your_program.sh` to fix this, check
[this PR](https://github.com/codecrafters-io/build-your-own-redis/pull/89/files) for details.
