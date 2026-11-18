This extension adds replication, so several Redis servers can keep copies of the
same data. Running a master and replicas on one machine requires a different
port for each. In this stage, you'll let your server start on a custom port.

### Leader-follower replication

[Leader-follower replication](https://redis.io/docs/latest/operate/oss_and_stack/management/replication/)
uses one server, the "master", to accept writes and other servers, the
"replicas", to maintain copies. The master sends changes to the replicas. Those
copies can serve reads, although they can lag behind the master's latest write.

### Custom port support

Servers listening on the same network address need different ports. Replication
itself comes later. This stage only changes where your server listens.

The `--port` flag passes the port number to the Redis server:

```bash
./your_program.sh --port <port_number>
```

The server then parses this argument and starts a TCP server on the specified
port.

If you don't provide a `--port` flag, the Redis server defaults to port `6379`.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port 6380
```

It'll then try to connect to your TCP server on the specified port number. If
the connection succeeds, you'll pass this stage.

### Notes

- The tester will pass a random port number to your program, so you can't
  hardcode the port number from the example above.
- If your repository was created before 5th Oct 2023, it's possible that your
  `./your_program.sh` script might not be passing arguments on to your program.
  You'll need to edit `./your_program.sh` to fix this, check
  [the launcher argument-forwarding fix](https://github.com/codecrafters-io/build-your-own-redis/pull/89/files)
  for details.
