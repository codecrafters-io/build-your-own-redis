Your blocking reads start after a known ID. In this stage, you'll let a client
pass `$` to wait for future entries while skipping the stream's existing
history.

### Using `$` as the starting ID

When your server handles `XREAD` with `$`, use the stream's current greatest ID
as the exclusive boundary for that request. Keep that boundary fixed while
waiting. Entries appended afterward can then satisfy the read.

For example, suppose we have two separate clients. The first client sends a
blocking `XREAD` command with `1000` as the timeout and `$` as the ID:

```bash
$ redis-cli XREAD BLOCK 1000 streams some_key $
```

Then, the second client adds an entry to the `some_key` stream.

```bash
$ redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
```

Similar to the behavior detailed in the earlier stages, if the command is sent
within `1000` milliseconds, the first client will be unblocked and get the new
entry:

```bash
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
         2) 1) temperature
            2) 37
            3) humidity
            4) 94
```

If not, the response will be a
[null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays).

```bash
(nil)
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then add an entry to a stream.

```bash
$ redis-cli XADD stream_key 0-1 temperature 96
```

Next, it will send `XREAD` with `BLOCK 0` and `$` as the ID.

```bash
$ redis-cli XREAD BLOCK 0 streams stream_key $
```

Using another connection, the tester will add an entry after a delay.

```bash
$ redis-cli XADD stream_key 0-2 temperature 95
```

Your server should respond with the following, as a RESP array:

```json
[["stream_key", [["0-2", ["temperature", "95"]]]]]
```

After that, the tester will send `XREAD` with `BLOCK 1000` and let the timeout
expire without adding another entry.

```bash
$ redis-cli XREAD BLOCK 1000 streams stream_key $
```

Your server should respond with a null array (`*-1\r\n`).
