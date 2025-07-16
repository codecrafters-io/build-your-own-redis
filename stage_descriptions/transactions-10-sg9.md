In this stage, you'll add support for handling failures within a transaction.

### Failures within transactions

When executing a transaction, if a command fails, the error is captured and returned within the response for `EXEC`. All other commands in
the transaction are still executed.

You can read more about this behaviour [in the official Redis docs](https://redis.io/docs/latest/develop/interact/transactions/#errors-inside-a-transaction).

Example:

```bash
$ redis-cli
> MULTI
OK
> SET foo xyz
QUEUED
> INCR foo
QUEUED
> SET bar 7
> EXEC
1) OK
2) (error) ERR value is not an integer or out of range
3) OK
```

In the example above, note that the error for the `INCR` command was returned as the second array element. The third command (`SET bar 7`) was
still executed successfully.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client, and send multiple commands using the same connection:

```bash
$ redis-cli
> SET foo abc
OK
> SET bar 41
OK
> MULTI
OK
> INCR foo
QUEUED
> INCR bar
QUEUED
> EXEC
1) (error) ERR value is not an integer or out of range
2) (integer) 42
```

The expected response for `EXEC` is a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays) of
the responses of the queued commands. The exact bytes will be:

```bash
*2\r\n-ERR value is not an integer or out of range\r\n:42\r\n
```

The tester will also verify that the last command was successfully executed and that the key `bar` exists:

```bash
$ redis-cli
> GET foo (expecting "$3\r\nabc\r\n" as the response)
> GET bar (expecting "$2\r\n42\r\n" as the response)
```

### Notes

- There are a subset of command failures (like syntax errors) that will cause a transaction to be aborted entirely. We won't
  cover those in this challenge.
