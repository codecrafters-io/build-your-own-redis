Your master handles `WAIT` with no replicas. In this stage, you'll count
replicas that completed synchronization when no write commands have been sent
yet.

### `WAIT` with connected replicas

In previous stages, we handled the case where no replicas were connected, and
the master could safely return `0`.

Now, we’ll consider the case where some replicas are connected. Each replica
will have completed the handshake and received the empty RDB file. But since no
write commands have been sent yet, the replication offset is still `0`.

For this stage's simplified zero-offset case, return the number of connected
replicas that completed the handshake and received the empty snapshot:

```bash
$ redis-cli WAIT 3 500
(integer) 7
$ redis-cli WAIT 7 500
(integer) 7
$ redis-cli WAIT 9 500
(integer) 7
```

In the example above, `7` replicas are connected. No matter how many replicas
the client asks for, the master will reply with the number of connected replicas
(`7`).

You can ignore both arguments (`<numreplicas> <timeout>`) for this stage. Later
stages replace this shortcut with acknowledgment and timeout handling after
writes.

### Tests

The tester will execute your program as a master like this:

```
./your_program.sh
```

It will then start **multiple** replicas that connect to your server. Each will
complete the handshake and expect to receive an empty RDB file.

It will then connect to your master as a client and send commands like this:

```bash
$ redis-cli WAIT 3 500 # (expecting 7 back)
$ redis-cli WAIT 7 500 # (expecting 7 back)
$ redis-cli WAIT 9 500 # (expecting 7 back)
```

The response to each of these commands should be encoded as a RESP integer
(i.e., `:7\r\n`).

### Notes

- The number of replicas created in this stage will be random, so you can't
  hardcode `7` as the response, like in the example above.
