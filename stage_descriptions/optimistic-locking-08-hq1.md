In this stage, you'll implement the `DISCARD` command and verify that it clears watched keys.

### The `DISCARD` Command

The [`DISCARD` command](https://redis.io/docs/latest/commands/discard/) aborts a transaction. It throws away all queued commands and exits transaction mode:

```bash
$ redis-cli
> MULTI
OK
> SET foo 100
QUEUED
> SET bar 200
QUEUED
> DISCARD
OK
```

The response is always `+OK\r\n`.

### Clearing Watch State

Just like `EXEC`, `DISCARD` also clears the connection's watched keys. This means a subsequent transaction on the same connection won't be affected by modifications that happened before the `DISCARD`.

```bash
$ redis-cli
# Client A
> WATCH foo
OK

# Client B
> SET foo 999
OK

# Client A
> MULTI
OK
> SET bar 200
QUEUED
> DISCARD
OK                     # watch state cleared

# Client A starts a new transaction (no WATCH active)
> MULTI
OK
> SET bar 200
QUEUED
> EXEC
1) OK                  # succeeds, DISCARD cleared the watch state
```

Without this cleanup, the second transaction would incorrectly fail because `foo` was modified before the `DISCARD`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn two clients. Using the first client, it will set two keys, watch them, and begin a transaction:

```bash
# Client 1
> SET foo 100
OK
> SET bar 200
OK
> WATCH foo bar
OK
> MULTI
OK
> SET bar 300
QUEUED
```

Using the second client, it will modify one of the watched keys:

```bash
# Client 2
> SET foo 400
OK
```

Back on the first client, the tester will discard the transaction, then immediately start and execute a new one:

```bash
# Client 1
> DISCARD
OK

# Client 1 starts a new transaction
> MULTI
OK
> SET bar 300
QUEUED
> EXEC
1) OK                  # succeeds, DISCARD cleared the watch state

# Client 2
> GET bar
"300"                  # confirms the second transaction's write took effect
```

The tester will verify that:

- `DISCARD` returns `+OK\r\n`
- `DISCARD` clears the connection's watch state
- The second transaction succeeds because the watched keys were cleared by `DISCARD`
- The second transaction's queued commands were applied

### Notes

- `DISCARD` should clear both the command queue and the watch state (watched keys and dirty flag).
- If you already handle watch state cleanup in `EXEC`, the `DISCARD` implementation should follow the same pattern.
