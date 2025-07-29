In this stage, you'll add support clearing watched keys on `DISCARD`.

### Tests

```
$ ./your_program.sh
```

The tester will spawn two clients.

Using the first client, it will set the value of two keys and, issue a `WATCH` command specifying both keys.

```bash
# Client 1
> SET foo 100 (Expecting "+OK\r\n")
> SET bar 200 (Expecting "+OK\r\n")
> WATCH foo bar
```

Using the second client, the tester will modify one of the watched keys.

```bash
# Client 2
> SET foo 200 (Expecting "+OK\r\n")
```

Using the first client, the tester will then try to execute a transaction

```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
> DISCARD (Expecting "+OK\r\n)
```

The transaction should abort with an OK response to the `DISCARD` command.

Now, using the first client, the tester will then try to execute a transaction

```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET bar 1000 (Expecting "+QUEUED\r\n")
> SET foo 2000 (Expecting "+QUEUED\r\n")
> EXEC (Expecting an array of responses for the queued commands)
```

The transaction should succeed.

Using the second client, the tester will check for the values of variables that were modified in the transaction.

```bash
# Client 2
> GET foo (Expecting "1000")
> GET bar (Expecting "2000")
```