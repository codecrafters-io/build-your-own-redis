Your master forwards writes to one replica. In this stage, you'll send the same
ordered stream to every connected replica that has completed synchronization.

### Command propagation

Once a replica completes the handshake and loads the RDB file, it is ready to
start receiving live updates from the master.

Every synchronized replica needs each subsequent write command on its own
replication connection. Each replica must receive those commands in the order
the master processed them.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It will then start **multiple** replicas that will each connect to your server,
complete the handshake, and receive the initial RDB file.

Next, the tester will send `SET` commands to the master from a separate client.

```bash
$ redis-cli SET foo 1
$ redis-cli SET bar 2
$ redis-cli SET baz 3
```

It will then assert that each replica received those commands in the correct
order.
