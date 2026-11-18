Your server recognizes `MULTI`. Executing queued commands comes later. In this
stage, you'll add the `EXEC` error response for a connection with no active
transaction.

### The `EXEC` command

The [EXEC](https://redis.io/docs/latest/commands/exec/) command executes all
commands queued in a transaction.

It returns an array of the responses of the queued commands.

Example usage:

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

### EXEC without MULTI

If the connection has no active transaction started by `MULTI`, `EXEC` returns
an error.

Example usage:

```bash
$ redis-cli EXEC
(error) ERR EXEC without MULTI
```

The returned value is a
[Simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors),
the exact bytes are `-ERR EXEC without MULTI\r\n`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client and run the
following commands:

```bash
$ redis-cli EXEC
```

The tester expects `-ERR EXEC without MULTI\r\n` as the response.
