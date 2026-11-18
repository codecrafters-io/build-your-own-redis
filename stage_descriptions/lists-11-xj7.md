Your `BLPOP` can wait indefinitely. In this stage, you'll support a positive
timeout so a client can stop waiting when no element arrives in time.

### `BLPOP` with a timeout

The timeout is measured in seconds and may have a fractional part. For example,
`0.5` means 500 milliseconds.

```bash
$ redis-cli BLPOP list_key 0.5
# (Blocks for 0.5 seconds)
```

If an element is not available on the list within this time, the server returns
a
[null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays)
(`*-1\r\n`). If an element is pushed to the list before the timeout, the command
unblocks and returns the list key and the popped element as a RESP array, just
like with previous stages.

A request finishes once it receives either result. A timed-out request must not
consume an element pushed later.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send a `BLPOP` command with a non-zero timeout:

```bash
$ redis-cli BLPOP list_key 0.1
# (Blocks for 0.1 seconds)
```

After the timeout expires, the tester will expect to receive a
[null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays)
(`*-1\r\n`) as the response.

The tester will also test the case where an element is appended to the list
before the timeout is reached. In this case, the response should be a RESP
encoded array like `["list_key", "foo"]` where `foo` is the added element.
