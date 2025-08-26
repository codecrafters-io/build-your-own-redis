In this stage, you'll add support for executing an empty transaction.

### Empty transactions

If [EXEC](https://redis.io/docs/latest/commands/exec/) is executed soon after [MULTI](https://redis.io/docs/latest/commands/multi/),
it returns an empty array.

The empty array signifies that no commands were queued, and that the transaction was executed successfully.

Example usage:

```bash
$ redis-cli
> MULTI
OK
> EXEC
(empty array)
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client and run the following commands:

```bash
$ redis-cli
> MULTI (expecting "+OK\r\n")
> EXEC (expecting "*0\r\n" as the response)
> EXEC (expecting "-ERR EXEC without MULTI\r\n" as the response)
```
