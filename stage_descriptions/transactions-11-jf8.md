Your server handles a transaction on one connection. In this stage, you'll allow
several clients to have open transactions at once, keeping each client's pending
commands separate.

### Multiple transactions

There can be multiple transactions open (i.e. `MULTI` has been called, but
`EXEC` has not been called yet) at the same time. Each transaction gets its own
command queue.

The database is shared across clients. Only the pending commands and transaction
state belong to individual connections. Executing one transaction must not
execute or discard another client's queue.

For example, say you started transaction 1 from one connection:

```bash
$ redis-cli
> MULTI
OK
> SET foo 41
QUEUED
> INCR foo
QUEUED
```

and started transaction 2 from another connection:

```bash
$ redis-cli
> MULTI
OK
> INCR foo
QUEUED
```

If you then run `EXEC` in transaction 1, you should see the following:

```bash
> EXEC
1) OK
2) (integer) 42
```

`OK` is the response to `SET foo 41`, and `42` is the response to `INCR foo`.

And for transaction 2, running `EXEC` should return:

```bash
> EXEC
1) (integer) 43
```

43 is the response to `INCR foo`. The key `foo` was updated to `42` by
transaction 1, and `INCR foo` further increments it to `43`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as multiple Redis clients, and send
multiple commands from each connection:

```bash
$ redis-cli
> MULTI
> INCR foo
> EXEC
```
