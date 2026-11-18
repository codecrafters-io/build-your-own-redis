You have commands whose effects can be observed with `GET`. In this stage,
you'll recognize `MULTI`, which starts a transaction on a client connection, and
acknowledge it with `+OK\r\n`.

### The `MULTI` command

The [MULTI](https://redis.io/docs/latest/commands/multi/) command starts a
transaction: a group of commands saved for later execution together.

After a `MULTI` command is executed, any further commands from the same
connection will be "queued" but not executed.

Example usage:

```bash
$ redis-cli
> MULTI
OK
> SET foo 41
QUEUED
> INCR foo
QUEUED
```

The queued commands can be executed using
[EXEC](https://redis.io/docs/latest/commands/exec/), which we'll cover in later
stages.

The example shows the eventual queueing behavior. Queueing and execution come in
later stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

The tester will then connect to your server as a Redis client and run the
following command:

```bash
$ redis-cli MULTI
```

The tester will expect `+OK\r\n` as the response.
