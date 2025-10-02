In this stage, you'll add support for propagating write commands to a single replica as a master.

### Command Propagation

After the replication handshake is complete and the master has sent the RDB file to the replica, the master starts propagating "write" commands to the replica.

Write commands are commands that modify the master's dataset, such as `SET` and `DEL`. Commands like `PING`, `ECHO`, etc., are not considered "write" commands, so they aren't propagated.

### The Propagation Process

Command propagation happens over the replication connection. This is the same connection that was used for the handshake.

The propagated commands are sent as RESP arrays. For example, if the master receives `SET foo bar` as a command from a client, it'll send `*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\nbar\r\n` to all connected replicas over their respective replication connections.

Replicas process commands received over the connection just like they would process commands received from a client, but with one difference: they don't send responses back to the master. They just process the command silently and update their state.

Similarly, the master doesn't wait for a response from the replica when propagating commands. It just sends the commands as they come in.

There is one exception to this "no response" rule: the `REPLCONF GETACK` command. We'll learn about this command in later stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It will then connect to your TCP server as a replica and complete the full handshake sequence covered in previous stages.

The tester will then wait for your server to send an RDB file.

Once the RDB file is received, the tester will send a series of write commands to your program (as a separate Redis client).

```bash
$ redis-cli SET foo 1
$ redis-cli SET bar 2
$ redis-cli SET baz 3
```

It will then assert that these commands were propagated to the replica in the correct order. The tester will expect to receive these commands encoded as RESP arrays and sent on the replication connection (the one used for the handshake).

### Notes

- Although replicas provide a `listening-port` during the handshake, it’s used only for [monitoring/logging purposes](https://github.com/redis/redis/blob/90178712f6eccf1e5b61daa677c5c103114bda3a/src/replication.c#L107-L130), not for propagation. Redis propagates commands over the same TCP connection that the replica initiated during the handshake.
- A true implementation would buffer the commands so that they can be sent to the replica after it loads the RDB file. For the purposes of this challenge, you can assume that the replica is ready to receive commands immediately after receiving the RDB file.
