In this stage, you'll add support for watching a non-existent key.

### Optimistic locking using `WATCH`
The WATCH command enables optimistic locking by monitoring non-existent exist keys at the time of the WATCH call. Example Usage:

```
# Client A
> WATCH foo
+OK

# Client B
> SET foo external
+OK

# Client A
> MULTI
+OK
> SET bar value
+QUEUED
> EXEC
*-1   # Transaction aborts because "foo" was created after WATCH
```

### Tests

The tester will execute your program like this:

```
$ ./your_program.sh
```

The tester will spawn two clients.

Using the first client, it will set the value of two keys and, issue a `WATCH` command specifying one of the keys

```bash
# Client 1
> WATCH foo
```

Using the second client, the tester will modify the watched key.

```bash
# Client 2
> SET foo 200 (Expecting "+OK\r\n")
```

Using the first client, the tester will attempt to execute an empty transaction.
```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET foo 300 (Expecting "+QUEUED\r\n")
> EXEC (Expecting "*-1\r\n")
```

The response to `EXEC` should be a RESP null array. 

Using the second client, the tester will retrieve the value of the watched key to check if the transaction was aborted.

```bash
# Client 2
> GET foo (Expecting bulk string "200")
```