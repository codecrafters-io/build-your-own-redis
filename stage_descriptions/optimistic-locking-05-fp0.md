In this stage, you'll add support for watching multiple keys.

### Multiple Keys in `WATCH` Command

The `WATCH` command can be used to watch multiple keys. After the watch command is issued, a transaction will fail if any one of the watched keys was modified before the execution of the `EXEC` command.

Example Usage:

```bash
# Client A
> WATCH foo bar
+OK

# Client B
> SET bar "300"
+OK

# Client A
> MULTI
+OK
> SET foo "400"
+QUEUED

> EXEC
*-1     # Transaction aborts due to watched key being modified
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn two clients.

Using the first client, it will set the value of two keys and, issue a `WATCH` command specifying both the keys:

```bash
# Client 1
> SET foo 100 (Expecting "+OK\r\n")
> SET bar 200 (Expecting "+OK\r\n")
> WATCH foo bar (Expecting "+OK\r\n") 
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
```

Using the second client, the tester will modify one of the watched keys.

```bash
# Client 2
> SET foo 200 (Expecting "+OK\r\n")
```

Using the first client, the tester will attempt to execute the ongoing transaction.
```bash
# Client 1
> EXEC (Expecting "*-1\r\n")
```

The response to `EXEC` should be a RESP null array. 

Using the second client, the tester will retrieve the value of the unwatched key to check if the transaction was aborted.

```bash
# Client 2
> GET bar (Expecting bulk string "200")
```