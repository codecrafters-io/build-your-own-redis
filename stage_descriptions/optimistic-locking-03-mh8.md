In this stage, you'll implement the `EXEC` command and add support for failing transactions when watched keys have been modified.

### Redis Transactions (Recap)

As a recap, [Redis transactions](https://redis.io/docs/latest/develop/using-commands/transactions/) allow clients to execute multiple commands as a single operation. The basic flow is:

1. `MULTI` - Enter transaction mode (Handled in previous stages)
2. `SET key value`, `INCR counter`, etc. - Commands are queued
3. `EXEC` - Execute all queued commands

For this stage, you'll implement the `EXEC` command and fail transactions when their watched keys have been modified.

### The `EXEC` Command

The [`EXEC` command](https://redis.io/docs/latest/commands/exec/) executes all commands that were queued after `MULTI`:

```bash
$ redis-cli
> SET foo 100
OK
> MULTI
OK
> SET foo 200
QUEUED
> SET bar 300
QUEUED
> EXEC
1) OK
2) OK
```

The response is a RESP array where each element is the result of a queued command, in the order they were queued.

### Failing Transactions With `WATCH`

When a connection has watched keys, `EXEC` needs to check whether any of those keys were modified by another client between the `WATCH` and the `EXEC`. If they were, the transaction is aborted:

```
Client A: WATCH foo             # Server tracks "foo" for Client A
Client A: MULTI
Client A: SET bar 300           # Queued
Client B: SET foo 999           # Modifies Client A's watched key
Client A: EXEC                  # Returns *-1\r\n, queued commands discarded
```

`EXEC` returns a RESP null array (`*-1\r\n`), and the queued commands are discarded without executing.

If none of the watched keys were modified, `EXEC` runs the transaction normally.

<!-- To make this work, your server needs to detect modifications across connections. Whenever it processes a write command, it should check if the modified key is being watched by any other connection. If it is, that connection's transaction should be marked as dirty so that its next `EXEC` knows to abort.

After `EXEC` completes (whether it succeeded or was aborted), clear the connection's watched keys and dirty state. -->

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn four clients and run two scenarios. 

In the first scenario, it modifies the watched key from a different client:

```bash
# Client 1
> SET foo 100        # → +OK
> SET bar 200        # → +OK
> WATCH foo          # → +OK
> MULTI              # → +OK
> SET bar 300        # → +QUEUED

# Client 2 modifies the watched key
> SET foo 200        # → +OK

# Client 1 tries to execute
> EXEC               # → *-1\r\n (null array, transaction aborted)
> GET bar            # → "200" (unchanged, SET bar 300 was never executed)
```

In the second scenario, it modifies a different key (not the watched one): 

```bash
# Client 3
> SET baz 100        # → +OK
> SET caz 200        # → +OK
> WATCH baz          # → +OK
> MULTI              # → +OK
> SET caz 400        # → +QUEUED

# Client 4 modifies a different key (not the watched one)
> SET caz 300        # → +OK

# Client 3 executes successfully
> EXEC               # → array of responses (baz was not modified)

# Client 4 verifies the transaction's write took effect
> GET caz            # → "400"
```

The tester will verify that:

- `EXEC` returns a RESP null array (`*-1\r\n`) when a watched key was modified by another client
- `EXEC` returns the array of command results when no watched keys were modified
- Queued commands are not executed when the transaction is aborted, and the original values are preserved
- Queued commands are executed when no watched keys are modified, and their writes take effect

### Notes

- It's not enough to compare the value of a watched key at `WATCH` time vs `EXEC` time. If a key was modified and then set back to its original value, the transaction should still abort. Track whether the key was touched, not whether its value changed.
- After `EXEC`, clear the connection's watch state (watched keys and dirty flag) regardless of whether the transaction succeeded or was aborted.
