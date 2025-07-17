**🚧 We're still working on instructions for this stage**. You can find notes on how the tester works below.

<!--
In this stage you will implement WAIT, when some replicas are connected to Master, and there have been commands propagated from master to replica. So the offset is NOT 0. In this case, the Master has to figure out how many replicas the previous write command has been successfully propagated to.
(The replicas will finish the sync handshake with Master, and process any commands you send it, but they WON'T send periodic ACKs, so you need to basically send a REPLCONF GETACK to get their current offset. )
-->

### Tests

The tester will execute your program as a master like this:

```
./your_program.sh
```

It'll then start **multiple** replicas that connect to your server. Each will complete the handshake and expect to receive an empty RDB file.

The tester will then connect to your master as a Redis client (not one of the replicas) and send multiple write commands interleaved
with `WAIT` commands:

```bash
$ redis-cli SET foo 123
$ redis-cli WAIT 1 500 # (must wait until either 1 replica has processed previous commands or 500ms have passed)

$ redis-cli SET bar 456
$ redis-cli WAIT 2 500 # (must wait until either 2 replicas have processed previous commands or 500ms have passed)
```

### Notes

- The `WAIT` command should return when either (a) the specified number of replicas have acknowledged the command, or (b) the timeout expires.
- The `WAIT` command should always return the number of replicas that have acknowledged the command, even if the timeout expires.
- The returned number of replicas might be lesser than or greater than the expected number of replicas specified in the `WAIT` command.
