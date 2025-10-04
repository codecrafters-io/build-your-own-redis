In this stage, you’ll begin implementing support for the `WAIT` command on the master.

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
$ redis-cli WAIT 3 5000
(integer) 2
```

Here, the client is asking the master to wait for `3` replicas (with a maximum timeout of 5000 ms). After the timeout passes, the master has only `2` replicas connected, so it immediately replies with `2` as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

For now, we’ll handle the simplest case: when the client needs `0` replicas and the master also has no replicas connected. In this case, `WAIT` should immediately return `0`.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then connect to your master and send:

```bash
$ redis-cli WAIT 0 60000
```

The tester will expect to receive `0` immediately (as a RESP integer), since no replicas are connected.
