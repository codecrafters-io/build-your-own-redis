In this stage, you'll add support for clearing watched keys on `EXEC`.

### Clearing Watched Keys on `EXEC`

After a client issues a `WATCH` command, the watched keys must be automatically cleared when `EXEC` is called.

This means that any future transaction from the same client should not be affected by key modifications that occurred prior to the new transaction.

This behavior ensures that `WATCH` only affects a single transaction and does not persist across multiple transaction cycles.

Example Usage:

```bash
# Client A
> WATCH foo
+OK

# Client B
> SET foo 100
+OK

# Client A
> MULTI
+OK
> SET bar 200
+QUEUED
> EXEC
*-1   # Transaction aborts because "foo" was created after WATCH

> MULTI
OK
> SET bar 200
OK
> SET foo 1000
OK
> EXEC
1) OK
2) OK # Transaction succeeds because the previous EXEC clears watched keys
```

### Tests

```bash
$ ./your_program.sh
```

The tester will spawn two clients.

Using the first client, it will set the value of two keys and, issue a `WATCH` command specifying one of the keys, and begin a transaction.

```bash
# Client 1
> SET foo 100 (Expecting "+OK\r\n")
> SET bar 200 (Expecting "+OK\r\n")
> WATCH foo (Expecting "+OK\r\n")
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
```

Using the second client, the tester will modify the watched key.

```bash
# Client 2
> SET foo 200 (Expecting "+OK\r\n")
```

Using the first client, the tester will then try to execute the transaction.

```bash
# Client 1
> EXEC (Expecting "*-1\r\n")
```

The transaction should abort with a RESP null array response to the `EXEC` command.

Now, using the first client, the tester will again execute the transaction.

```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
> EXEC (Expecting an array of responses for the queued commands)
```

The transaction should succeed.

Using the second client, the tester will check for the values of the key that was modified in the transaction.

```bash
# Client 1
> GET bar (Expecting "300")
```

### Notes

- In this stage, you'll only implement clearing the watched keys on `EXEC`. We'll get to clearing the watched keys on `DISCARD` in the later stages.

