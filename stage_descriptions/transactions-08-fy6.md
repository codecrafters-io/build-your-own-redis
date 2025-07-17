In this stage, you'll add support for executing a transaction that contains multiple commands.

### Executing a transaction

When the [EXEC](https://redis.io/docs/latest/commands/exec/) command is sent within a transaction,
all commands queued in that transaction are executed.

The response to [EXEC](https://redis.io/docs/latest/commands/exec/) is an array of the responses of the queued commands.

Example:

```bash
$ redis-cli
> MULTI
OK
> SET foo 41
QUEUED
> INCR foo
QUEUED
> EXEC
1) OK
2) (integer) 42
```

In the above example, `OK` is the response of the `SET` command, and `42` is the response of the `INCR` command.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client, and send multiple commands using the same connection:

```bash
$ redis-cli MULTI
> SET foo 6 (expecting "+QUEUED\r\n")
> INCR foo (expecting "+QUEUED\r\n")
> INCR bar (expecting "+QUEUED\r\n")
> GET bar (expecting "+QUEUED\r\n")
> EXEC (expecting an array of responses for the queued commands)
```

Since the transaction was executed, the key `foo` should exist and have the value `7`.

The tester will verify this by running a GET command:

```bash
$ redis-cli GET foo (expecting "7" encoded as a bulk string)
```
