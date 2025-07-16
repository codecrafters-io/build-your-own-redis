In this stage, you'll add support for the DISCARD command.

### The DISCARD command

[DISCARD](https://redis.io/docs/latest/commands/discard/) is used to abort a transactions. It discards all commands queued in a transaction,
and returns `+OK\r\n`.

Example:

```bash
$ redis-cli
> MULTI
OK
> SET foo 41
QUEUED
> DISCARD
OK
> DISCARD
(error) ERR DISCARD without MULTI
```

In the above example, note that the first `DISCARD` returns `OK`, but the second `DISCARD` returns an error since the transaction was aborted.

### DISCARD

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client, and send multiple commands using the same connection:

```bash
$ redis-cli
> MULTI
> SET foo 41 (expecting "+QUEUED\r\n")
> INCR foo (expecting "+QUEUED\r\n")
> DISCARD (expecting "+OK\r\n")
> GET foo (expecting "$-1\r\n" as the response)
> DISCARD (expecting "-ERR DISCARD without MULTI\r\n" as the response)
```
