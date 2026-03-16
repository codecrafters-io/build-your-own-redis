In this stage, you'll add support for failing transactions based on the modification of watched keys.

### Optimistic locking using `WATCH`

When a client has issued `WATCH` on one or more keys and then runs `EXEC`, the transaction must fail if any watched key was modified by another client after the `WATCH` and before the `EXEC`. In that case, `EXEC` returns a RESP null array (`*-1\r\n`) and the queued commands are not executed.

If none of the watched keys were modified, `EXEC` runs the transaction as usual.


### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will spawn four clients.

Using the first client, it will set the value of two keys and, issue a `WATCH` command specifying one of the keys, and begin a transaction:

```bash
# Client 1
> SET foo 100 (Expecting "+OK\r\n")

> SET bar 200 (Expecting "+OK\r\n")

> WATCH foo (Expecting "+OK\r\n") 

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
# Client 1
> EXEC (Expecting "*-1\r\n")
```

The response to `EXEC` should be a RESP null array because the watched variable `foo` was modified by Client 2 before the execution of `EXEC`.

Using the first client, the tester will retrieve the value of the unwatched key to check if the transaction was aborted.

```bash
# Client 1
> GET bar (Expecting bulk string "200")
```

Now, using the third client, the tester will set the value of two keys and, issue a `WATCH` command specifying one of the keys:

```bash
# Client 3
> SET baz 100 (Expecting "+OK\r\n")
> SET caz 200 (Expecting "+OK\r\n")
> WATCH baz (Expecting "+OK\r\n") 
> MULTI (Expecting "+OK\r\n")
> SET caz 400 (Expecting "+QUEUED\r\n")
```

Using the fourth client, the tester will modify the value of the variable that was not watched.

```bash
# Client 4
> SET caz 300 (Expecing "+OK\r\n")
```

Using the third client, the tester will attempt to execute the ongoing transaction.

```bash
# Client 3
> EXEC (Expecting an array of responses for the queued commands)
```

The response to `EXEC` should be its usual response because the watched variable was not modified.

Using the fourth client, the tester will retrieve the value of modified variable to check if the transaction succeeded.

```bash
# Client 4
> GET caz (Expecting bulk string "400")
```

### Notes

- The transaction should fail if the watched key was modified before the execution of `EXEC`.

- It is not enough to check the equality of values of watched keys at the time of `WATCH` and `EXEC`. If the watched key was *touched*, it should be marked as modified, even if its value was restored to its original value.
