In this stage, you'll verify that watched keys are cleared after `EXEC`.

### Clearing Watched Keys on `EXEC`

`WATCH` is meant to protect a single transaction. Once `EXEC` runs (whether the transaction succeeds or aborts), the connection's watch state should be cleared. Without this cleanup, stale watch state would leak into future transactions on the same connection, causing them to fail for reasons that have nothing to do with them.

```bash
$ redis-cli
# Client A
> WATCH foo
OK

# Client B
> SET foo 100
OK

# Client A
> MULTI
OK
> SET bar 200
QUEUED
> EXEC
*-1\r\n                # transaction aborted, foo was modified

# Client A starts a new transaction (no WATCH this time)
> MULTI
OK
> SET bar 200
QUEUED
> EXEC
1) OK                  # succeeds, previous EXEC cleared the watch state
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn two clients. Using the first client, it will set two keys, watch one of them, and begin a transaction:

```bash
# Client 1
> SET foo 100
OK
> SET bar 200
OK
> WATCH foo
OK
> MULTI
OK
> SET bar 300
QUEUED
```

Using the second client, it will modify the watched key:

```bash
# Client 2
> SET foo 200
OK
```

Back on the first client, the tester will execute the transaction (expecting it to abort), then immediately start and execute a second transaction:

```bash
# Client 1
> EXEC
*-1\r\n                # first transaction aborted

# Client 1 starts a new transaction
> MULTI
OK
> SET bar 300
QUEUED
> EXEC
1) OK                  # second transaction succeeds

# Client 2
> GET bar
"300"                  # confirms the second transaction's write took effect
```

The tester will verify that:

- The first `EXEC` returns a RESP null array (`*-1\r\n`) because the watched key was modified
- The second `EXEC` succeeds because the first `EXEC` cleared the watch state
- The second transaction's queued commands were applied

### Notes

- If you have already cleared the watch state after `EXEC` (as suggested in an earlier stage), this stage should pass without additional changes.
- Clearing watched keys on `DISCARD` will be handled in a later stage.
