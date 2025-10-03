In this stage, you’ll begin implementing support for the `WAIT` command on the master.

<!--
In the next 3 stages, you will implement the WAIT command on your master.
The WAIT command is used to find out how many replicas a write command was propagated to, with the replica ACKing it back. This way we can know how durable the write was. As we haven't implemented periodic ACKs from the replica, in this stage, for WAIT, the master has to send a GETACK to the replica, if the replica replies back with the proper offset, before the WAIT expires, the master can count that replica's write to be a success.

In this stage you will implement WAIT, when exactly 0 replicas are connected to Master. The Master can just return 0 asap. This way we will gently dive into the implementation.
-->

### The `WAIT` command

The `WAIT` command is used to check how many replicas have acknowledged a write command. This allows a client to measure the durability of a write command before considering it successful.

The command format is: 

```bash
WAIT <numreplicas> <timeout>
```

Here's what each argument means:

- `<numreplicas>`: The minimum number of replicas that must acknowledge the write command.
- `<timeout>`: The maximum time (in milliseconds) the client is willing to wait.

For example:

```bash
$ redis-cli WAIT 0 60000
(integer) 0
```

Here, the client is asking the master to wait for `0` replicas (with a maximum timeout of 60,000 ms). Since no replicas are connected, the master immediately replies with `0`.

The return value is a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

In a full implementation, the master would send a `REPLCONF GETACK *` to each replica, then wait for acknowledgements (ACKs) up to the specified timeout. If a replica responds with an ACK that matches the master’s current replication offset, that replica is counted as having received the write.

We’ll build this up step by step over the next few stages.

For now, we’ll handle the simplest case: when the master has no replicas connected. In this case, `WAIT` should immediately return `0`, since there are no replicas to acknowledge anything.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

A redis client will then connect to your master and send `WAIT 0 60000`:

```bash
$ redis-cli WAIT 0 60000
```

It'll expect to receive `0` back immediately, since no replicas are connected.

### Notes

- You can hardcode `0` as the response for the WAIT command in this stage. We'll get to tracking the number of replicas and responding
  accordingly in the next stages.
