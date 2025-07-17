**🚧 We're still working on instructions for this stage**. You can find notes on how the tester works below.

<!--
In the next 3 stages, you will implement the WAIT command on your master.
The WAIT command is used to find out how many replicas a write command was propagated to, with the replica ACKing it back. This way we can know how durable the write was. As we haven't implemented periodic ACKs from the replica, in this stage, for WAIT, the master has to send a GETACK to the replica, if the replica replies back with the proper offset, before the WAIT expires, the master can count that replica's write to be a success.

In this stage you will implement WAIT, when exactly 0 replicas are connected to Master. The Master can just return 0 asap. This way we will gently dive into the implementation.
-->

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
