**🚧 We're still working on instructions for this stage**. You can find notes on how the tester works below.

<!--

In this stage you will implement WAIT, when some replicas are connected to Master, but there have been NO commands propagated from master to replica. So offset is essentially 0. In this case, the Master can just return the count of `connected_slaves` asap.  (The replicas will finish the sync handshake with Master, so they are actually connnected.)
(The master will return the count of `connected_slaves` no matter how many replicas we pass in the WAIT command parameter. As the offset is 0, it knows all replicas are in sync.)

-->

### Tests

The tester will execute your program as a master like this:

```
./your_program.sh
```

It'll then start **multiple** replicas that connect to your server. Each will complete the handshake and expect to receive an empty RDB file.

It'll then connect to your master as a Redis client (not one of the replicas) and send commands like this:

```bash
$ redis-cli WAIT 3 500 # (expecting 7 back)
$ redis-cli WAIT 7 500 # (expecting 7 back)
$ redis-cli WAIT 9 500 # (expecting 7 back)
```

The response to each of these commands should be encoded as a RESP integer (i.e. `:7\r\n`).

### Notes

- Even if WAIT is called with a number lesser than the number of connected replicas, the master should return the count of connected replicas.
- The number of replicas created in this stage will be random, so you can't hardcode `7` as the response like in the example above.
