In this stage, you'll extend your implementation of the master to support propagating commands to multiple replicas.

### Command Propagation (Recap)

Once a replica completes the handshake and loads the RDB file, it is ready to start receiving live updates from the master. 

Every write command executed on the master must be forwarded to all connected replicas, not just one. This ensures that every replica stays in sync with the master.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It will then start **multiple** replicas that will each connect to your server, complete the handshake sequence, and receive the initial RDB file.

Next, the tester will send `SET` commands to the master from a separate client.

```bash
$ redis-cli SET foo 1
$ redis-cli SET bar 2
$ redis-cli SET baz 3
```

It will then assert that each replica received those commands in the correct order.
