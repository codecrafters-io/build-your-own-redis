So far, clients receive write responses without knowing how far replicas have
caught up. In this stage, you'll introduce `WAIT` on the master, starting with a
request for zero acknowledgments when no replicas are connected.

### The `WAIT` command

`WAIT` reports how many replicas have acknowledged the preceding writes from the
same client connection. It can wait for a requested count, but an acknowledgment
alone doesn't prove the data was saved to disk.

The command format is:

```bash
WAIT <numreplicas> <timeout>
```

Here's what each argument means:

- `<numreplicas>`: The minimum number of replicas that must acknowledge all
  previous write commands.
- `<timeout>`: The maximum wait in milliseconds. A value of `0` means no
  timeout.

For example:

```bash
$ redis-cli WAIT 3 5000
(integer) 2
```

Here, the client requests `3` acknowledgments with a timeout of 5000 ms. If only
`2` replicas have acknowledged the writes by the timeout, the reply is the
[RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers)
`:2\r\n`. RESP integers begin with `:` and end with CRLF.

For now, only the zero-replica case is required. When the client requests `0`
replicas and none are connected, return `:0\r\n` immediately, regardless of the
timeout argument.

We'll get to tracking the number of replicas and responding accordingly in later
stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then connect to your master and send:

```bash
$ redis-cli WAIT 0 60000
```

The tester will expect to receive `0` immediately (as a RESP integer), since no
replicas are connected.
