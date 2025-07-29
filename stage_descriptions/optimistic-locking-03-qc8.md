In this stage, you'll add support for failing transactions after a `WATCH` command is issued.

### Optimistic locking using `WATCH`
The `WATCH` command enables optimistic locking by monitoring the watched key for changes.
If the watched key is modified after `WATCH` is called and before `EXEC` is run, the transaction is aborted.

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

> SET foo bar
QUEUED

> EXEC
(nil)     # Transaction aborts due to watched key being modified
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
> SET foo 100 (Expecting "+OK\r\n")
> SET bar 200 (Expecting "+OK\r\n")
> WATCH foo
```

Using the second client, the tester will update the value of the watched variable.
```bash
# Client 2
> SET foo 200 (Expecing "+OK\r\n")
```

Using the first client, the tester will attempt to execute a transaction.
```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
> EXEC (Expecting "*-1\r\n")
```

The response to `EXEC` should be a RESP null array. 

Using the first client, the tester will retrieve the value of unwatched key to check if the transaction was aborted.

```bash
# Client 1
> GET bar (Expecting bulk string "200")
```

Using the second client, the tester will attempt to execute a transaction.
```bash
# Client 2
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
> PING (Expecting "+QUEUED\r\n")
> EXEC (Expecting an array of responses for the queued commands)
```
The response to the `EXEC` command should be its usual response because `WATCH` command was not issued from this client previously.


Using the first client, the tester will retrieve the value of unwatched key to check if the transaction succeeded.

```bash
# Client 1
> GET bar (Expecting bulk string "300")
```


### Notes

- In this stage, you will implement aborting a transaction if a `WATCH` command was issued by the client previously.
    - Assume that the watched variable is guaranteed to be modified.

- If a `WATCH` command was not issued previously by a client, the transaction execution should continue with its usual operation.

- We will get to implementing failing transactions based on modification of watched keys in the next stage.