In this stage, you'll add support for a non-zero timeout for the `BLPOP` command.

### The `BLPOP` Command with a Non-Zero Timeout

The `BLPOP` command can take a timeout duration in seconds. 

```bash
$ redis-cli BLPOP list_key 2
# (Blocks for 2 seconds)
```

If an element is not available on the list within this time, the server returns a [null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays) (`*-1\r\n`). If an element is pushed to the list before the timeout, the command unblocks and returns the list key and the popped element as a RESP array, just as in the previous stage.

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

After the timeout expires, the tester will expect to receive a [null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays) (`*-1\r\n`) as the response.

The tester will also test the case where an element is appended to the list before the timeout is reached. In this case, the response should be a RESP encoded array like `["list_key", "foo"]` where `foo` is the added element. 
