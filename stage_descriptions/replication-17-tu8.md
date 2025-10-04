In this stage, you’ll extend your `WAIT` implementation to handle the case where replicas are connected, but no commands have been sent.

### `WAIT` with connected replicas

In a previous stage, we handled the case where no replicas were connected, and the master could safely return `0`.

Now, we’ll consider the case where some replicas are connected. Each replica will have completed the handshake and received the empty RDB file. But since no write commands have been sent yet, the replication offset is still `0`.

In this situation, the master will return the number of connected replicas, since it knows they are all in sync at offset `0`:

```bash
$ redis-cli WAIT 3 500
(integer) 7
$ redis-cli WAIT 7 500
(integer) 7
$ redis-cli WAIT 9 500
(integer) 7
```

In the example above, `7` replicas are connected. No matter how many replicas the client asks for, the master will reply with the number of connected replicas (`7`) once the timeout has passed.

For this stage, you can ignore both arguments (`<numreplicas> <timeout>`) and simply return the number of connected replicas.

### Tests

The tester will execute your program as a master like this:

```
./your_program.sh
```

It will then start **multiple** replicas that connect to your server. Each will complete the handshake and expect to receive an empty RDB file.

It will then connect to your master as a client and send commands like this:

```bash
$ redis-cli WAIT 3 500 # (expecting 7 back)
$ redis-cli WAIT 7 500 # (expecting 7 back)
$ redis-cli WAIT 9 500 # (expecting 7 back)
```

The response to each of these commands should be encoded as a RESP integer (i.e., `:7\r\n`).

### Notes

- Even if `WAIT` is called with a number less than the number of connected replicas, the master should return the count of connected replicas.
- The number of replicas created in this stage will be random, so you can't hardcode `7` as the response, like in the example above.
