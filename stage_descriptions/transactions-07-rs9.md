Your server can start and finish an empty transaction. In this stage, you'll
hold ordinary commands received after `MULTI` for later execution, without
changing the database when they are queued.

### Queuing commands

After [MULTI](https://redis.io/docs/latest/commands/multi/), ordinary commands
such as `SET` and `INCR` belong to that connection's transaction.
[EXEC](https://redis.io/docs/latest/commands/exec/) controls execution of the
queue. It isn't itself a queued data command.

The response to each queued command is `+QUEUED\r\n` (that's `QUEUED` encoded as
a
[Simple String](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings)).

Example:

```bash
$ redis-cli
> MULTI
OK
> SET foo 41
QUEUED
> INCR foo
QUEUED

... (and so on, until EXEC is executed)
```

When commands are queued, they should not be executed or alter the database in
any way.

If `foo` was missing before the example, it remains missing while these commands
are queued. Executing the queued commands is a task for later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client, and send multiple
commands using the same connection:

```bash
$ redis-cli
> MULTI
> SET foo 41 (expecting "+QUEUED\r\n")
> INCR foo (expecting "+QUEUED\r\n")
```

Since these commands were only "queued", the key `foo` should not exist yet. The
tester will verify this by creating another connection and sending this command:

```bash
$ redis-cli GET foo (expecting `$-1\r\n` as the response)
```
