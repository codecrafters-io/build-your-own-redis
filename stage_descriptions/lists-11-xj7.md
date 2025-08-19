In this stage, you will add support for a non-zero timeout duration for the `BLPOP` command.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send a BLPOP command with a non-zero timeout:

```bash
$ redis-cli BLPOP list_key 0.1
# (Blocks for 0.1 seconds)
```

After the timeout expires, the tester will expect to receive a [null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays) (`*-1\r\n`) as the response.

The tester will also test the case where an element is appended to the list before the timeout is reached. In this case, the response should be a RESP encoded array like `["list_key", "foo"]` where `foo` is the added element. 