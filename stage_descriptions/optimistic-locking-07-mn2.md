In this stage, you'll add support for the `UNWATCH` command.

### The `UNWATCH` command

The `UNWATCH` command is used to flush all the previously watched keys.

Example Usage:

```
$ redis-cli
> WATCH foo
OK
> UNWATCH
OK
```

After issuing the `UNWATCH` command, any transaction that is executed after it should not fail if the watched key was modified.

### Tests
The tester will execute your program like this:

```
$ ./your_program.sh
```

The tester will spawn two clients.

Using the first client, it will set the value of two keys and, issue a `WATCH` command specifying both the keys.

```bash
# Client 1
> SET foo 100 (Expecting "+OK\r\n")
> SET bar 200 (Expecting "+OK\r\n")
> WATCH foo bar
```

Using the second client, the tester will modify the watched key.

```bash
# Client 2
> SET foo 200 (Expecting "+OK\r\n")
```

Using the first client, the tester will issue a `UNWATCH` command, so that the first client stops watching the keys that were being previously watched.
```bash
# Client 1
> UNWATCH (Expecting "+OK\r\n")
```

Using the first client, the tester will then try to execute a transaction

```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
> SET foo 400 (Expecting "+QUEUED\r\n")
> EXEC (Expecting an array of responses for the queued commands)
```

Using the second client, the tester will retrieve the values of the variables to check if the transaction was executed successfully.

```bash
# Client 2
> GET foo (Expecting "400")
> GET bar (Expecting "300")
```