Your `XREAD BLOCK` can wait up to a deadline. In this stage, you'll interpret
`BLOCK 0` as waiting without a timeout, so a quiet stream doesn't end the
client's read.

### Indefinite blocking in `XREAD`

As a recap, the `XREAD` command supports blocking using the
`BLOCK <milliseconds>` parameter.

If `milliseconds` is `0` and no matching entries are available, wait until an
entry with an ID greater than the requested ID is available. Existing matching
entries still return immediately.

For example, consider two separate clients. The first client sends a blocking
`XREAD` command with `0` as the time passed in:

```bash
$ redis-cli XREAD BLOCK 0 streams some_key 1526985054069-0
```

This client will be blocked indefinitely until a new entry is added.

If the second client adds an entry to the stream at any time:

```bash
$ redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
```

The first client's `XREAD` command will immediately unblock and respond with the
new entry:

```bash
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
         2) 1) temperature
            2) 37
            3) humidity
            4) 94
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

Next, the tester will send an indefinite blocking `XREAD` command to your
server:

```bash
$ redis-cli XREAD BLOCK 0 streams stream_key 0-1
```

Through another connection, it will then add an entry. The waiting read must
remain pending until matching data arrives:

```bash
$ redis-cli XADD stream_key 0-2 temperature 95
```

Your server should respond with the following, encoded as a RESP array:

```json
[["stream_key", [["0-2", ["temperature", "95"]]]]]
```
