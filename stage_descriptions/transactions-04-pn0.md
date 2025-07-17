In this stage, you'll add support for the `MULTI` command.

### The MULTI command

The [MULTI](https://redis.io/docs/latest/commands/multi/) command starts a transaction.

After a `MULTI` command is executed, any further commands from the same connection will be "queued" but not executed.

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

The queued commands can be executed using [EXEC](https://redis.io/docs/latest/commands/exec/), which we'll cover in later stages.

In this stage, you'll just add support for handling the `MULTI` command and returning `+OK\r\n`. We'll get to queueing commands in later stages.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

The tester will then connect to your server as a Redis client and run the following command:

```bash
$ redis-cli MULTI
```

The tester will expect `+OK\r\n` as the response.

### Notes

- We'll test queueing commands & executing a transaction in later stages.
