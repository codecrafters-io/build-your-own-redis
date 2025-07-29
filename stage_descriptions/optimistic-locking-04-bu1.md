In this stage, you'll add support for failing transactions based on the modification of watched keys.

### Tests

The tester will execute your program like this:

```
$ ./your_program.sh
```

The tester will spawn four clients.

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
> SET foo 100 (Expecing "+OK\r\n")
```

Using the first client, the tester will attempt to execute a transaction.
```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET bar 300 (Expecting "+QUEUED\r\n")
> EXEC (Expecting "*-1\r\n")
```

The response to `EXEC` should be a RESP null array. Please note that even if the key's value was restored to its original value before the `WATCH` command was issued, the transaction will still fail, because the key was **modified**.

Using the first client, the tester will retrieve the value of unwatched key to check if the transaction was aborted.

```bash
# Client 1
> GET bar (Expecting bulk string "200")
```
---

Using the third client, the tester will set the value of two keys and, issue a `WATCH` command specifying one of the keys

```bash
# Client 1
> SET baz 100 (Expecting "+OK\r\n")
> SET caz 200 (Expecting "+OK\r\n")
> WATCH baz
```

Using the fourth client, the tester will modify the value of the **un-watched** variable.
```bash
# Client 2
> SET caz 200 (Expecing "+OK\r\n")
```

Using the third client, the tester will attempt to execute a transaction.
```bash
# Client 1
> MULTI (Expecting "+OK\r\n")
> SET caz 400 (Expecting "+QUEUED\r\n")
> EXEC (Expecting an array of responses for the queued commands)
```

The response to `EXEC` should be its usual response because the watched variable was un-modified.

Using the fourth client, the tester will retrieve the value of the unwatched variable to check if the transaction succeeded.

```bash
# Client 1
> GET caz (Expecting bulk string "400")
```


### Notes

- The transaction should fail if the watched key was modified before the execution of `EXEC`.
- It is not enough to check the equality of values of watched keys at the time of `WATCH` and `EXEC`. If the watched key was modified, it should be marked as modified, even if its value was restored to its original value before the `WATCH` command.