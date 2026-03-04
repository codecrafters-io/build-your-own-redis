In this stage, you'll add support for watching a missing key.

### Watching Non-existent Keys

The `WATCH` command enables optimistic locking by monitoring non-existent keys at the time of the `WATCH` call. Example Usage:

```bash
# Client A
> WATCH foo
+OK

# Client B
> SET foo "300"
+OK

# Client A
> MULTI
+OK
> SET bar "500"
+QUEUED
> EXEC
*-1   # Transaction aborts because "foo" was created after WATCH
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn two clients.

Using the first client, it will issue a `WATCH` command specifying a key:

```bash
# Client 1
> WATCH foo
```

Using the second client, the tester will set the value of the watched key.

```bash
# Client 2
> SET foo 200 (Expecting "+OK\r\n")
```

Using the first client, the tester will attempt to execute a transaction involving setting the watched key's value.

```bash
# Client 1
> MULTI (Expecting "+OK\r\n")

> SET foo 300 (Expecting "+QUEUED\r\n")

> EXEC (Expecting "*-1\r\n")
```

The response to `EXEC` should be a RESP null array. 

Using the first client, the tester will retrieve the value of the watched key to check if the transaction was aborted.

```bash
# Client 1
> GET foo (Expecting bulk string "200")
```
