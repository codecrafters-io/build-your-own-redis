In this stage, you'll add support for failing transactions after a `WATCH` command is issued.

### Failing Transaction in Case of Watched Keys

The `WATCH` command enables optimistic locking by monitoring the watched key for changes. If the watched key is modified after `WATCH` is called and before `EXEC` is run, the transaction is aborted.

Even if the value of the watched key is changed, and reverted back to its original value before the `WATCH` command was issued, it is still considered modified, because it was **touched**.

This facilitates the optimistic locking in Redis. Example Usage:

```bash
# Client A
> SET foo 100
OK

> WATCH foo
OK

# Client B
> SET foo 200
OK

# Client A
> MULTI
OK

> SET foo 300
QUEUED

> EXEC
(nil)     # Transaction aborts because the watched key was modified by Client B
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn two clients.

Using the first client, it will set the value of two keys and, issue a `WATCH` command specifying one of the keys, and initiate a transaction:

```bash
# Client 1
> SET foo 100 (Expecting "+OK\r\n")

> SET bar 200 (Expecting "+OK\r\n")

> WATCH foo

> MULTI (Expecting "+OK\r\n")

> SET bar 300 (Expecting "+QUEUED\r\n")
```

Using the second client, the tester will update the value of the watched variable.

```bash
# Client 2
> SET foo 200 (Expecting "+OK\r\n")
```

Using the first client, the tester will attempt to execute the ongoing transaction.

```bash
> EXEC (Expecting "*-1\r\n")
```

The response to `EXEC` should be a RESP null array. 

Using the first client, the tester will retrieve the value of the unwatched key to check if the transaction was aborted.

```bash
# Client 1
> GET bar (Expecting bulk string "200")
```

Using the second client, the tester will attempt to execute a new transaction.
```bash
# Client 2
> MULTI (Expecting "+OK\r\n")

> SET bar 300 (Expecting "+QUEUED\r\n")

> PING (Expecting "+QUEUED\r\n")

> EXEC (Expecting an array of responses for the queued commands)
```

The response to the `EXEC` command should be its usual response because no keys were watched using this client.

Using the first client, the tester will retrieve the value of the unwatched key to check if the transaction succeeded.

```bash
# Client 1
> GET bar (Expecting bulk string "300")
```

### Notes

- In this stage, implement aborting a transaction only when a watched key has been modified by another client. It is guaranteed that the tester will modify the watched variable using another client.

- We will get to implementing failing transactions based on the condition that the watched variable is actually modified, in the later stages.

- If no keys are watched by the client, the transaction execution should continue with its usual operation.