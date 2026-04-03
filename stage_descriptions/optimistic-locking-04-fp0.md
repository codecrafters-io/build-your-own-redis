In this stage, you'll add support for watching multiple keys.

### Watching Multiple Keys

In previous stages, you only needed to handle `WATCH` with a single key. The command also accepts multiple keys at once:

```bash
$ redis-cli
> WATCH foo bar baz
OK
```

All the keys are monitored for changes. If any one of them is modified by another client before `EXEC`, the transaction aborts:

```
Client A: WATCH foo bar          # Monitor both keys
Client B: SET bar 300            # Modifies one of the watched keys
Client A: MULTI
Client A: SET foo 400            # Queued
Client A: EXEC                   # Transaction aborted, bar was modified
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn two clients. Using the first client, it will set two keys, watch both of them, and begin a transaction:

```bash
# Client 1
> SET foo 100        # → OK
> SET bar 200        # → OK
> WATCH foo bar      # → OK
> MULTI              # → OK
> SET bar 300        # → QUEUED
```

Using the second client, it will modify one of the watched keys:

```bash
# Client 2
> SET foo 200        # → OK
```

Back on the first client, the tester will execute the transaction and verify the abort:

```bash
# Client 1
> EXEC               # → *-1\r\n (transaction aborted)

# Client 2
> GET bar            # → "200" (unchanged, transaction had no effect)
```

The tester will verify that:

- `WATCH` with multiple keys returns `+OK\r\n`
- `EXEC` returns a RESP null array (`*-1\r\n`) when any one of the watched keys was modified
- The aborted transaction's queued commands had no effect
