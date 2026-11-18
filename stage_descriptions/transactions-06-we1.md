Your server rejects `EXEC` outside a transaction. In this stage, you'll handle
`MULTI` followed by `EXEC` with no queued commands, and finish that transaction
so the connection returns to normal command handling.

### Empty transactions

If no commands were queued between
[MULTI](https://redis.io/docs/latest/commands/multi/) and
[EXEC](https://redis.io/docs/latest/commands/exec/), `EXEC` returns the empty
RESP array `*0\r\n`.

The empty array means the transaction completed with no command results. A
second `EXEC` on that connection must return `-ERR EXEC without MULTI\r\n` until
another transaction starts.

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

The tester will then connect to your server as a Redis client and run the
following commands:

```bash
$ redis-cli
> MULTI (expecting "+OK\r\n")
> EXEC (expecting "*0\r\n" as the response)
> EXEC (expecting "-ERR EXEC without MULTI\r\n" as the response)
```
