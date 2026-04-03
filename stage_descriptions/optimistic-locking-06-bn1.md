In this stage, you'll add support for the `UNWATCH` command.

### The `UNWATCH` Command

The [`UNWATCH` command](https://redis.io/docs/latest/commands/unwatch/) clears all watched keys for the current connection. Any keys previously registered with `WATCH` are no longer monitored, so future transactions won't be affected by changes to those keys.

```bash
$ redis-cli
> WATCH foo bar
OK
> UNWATCH
OK
```

The response is always `+OK\r\n`.

After `UNWATCH`, even if the previously watched keys are modified by another client, the next transaction will execute normally.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn two clients. Using the first client, it will set two keys and watch them:

```bash
# Client 1
> SET foo 100
OK
> SET bar 200
OK
> WATCH foo bar
OK
```

Using the second client, it will modify one of the watched keys:

```bash
# Client 2
> SET foo 200
OK
```

Back on the first client, the tester will unwatch all keys, then start and execute a transaction:

```bash
# Client 1
> UNWATCH
OK
> MULTI
OK
> SET foo 400
QUEUED
> EXEC
1) OK                  # transaction succeeds, keys were unwatched

# Client 2
> GET foo
"400"                  # confirms the transaction's write took effect
```

The tester will verify that:

- `UNWATCH` returns `+OK\r\n`
- `EXEC` succeeds even though a previously watched key was modified, because `UNWATCH` cleared the watch state
- The transaction's queued commands were applied

### Notes

- `UNWATCH` takes no arguments. It always clears all watched keys for the connection.
- `UNWATCH` should work even if no keys were previously watched (it just returns `+OK\r\n`).
