 In this stage, you’ll extend your `XREAD` implementation to support blocking.

### Understanding Blocking in `XREAD`

By default, the `XREAD` command is synchronous: it returns data immediately if entries are available, or an empty response if not. 

However, with the optional `BLOCK` parameter, clients can wait for new data to arrive.

The syntax looks like this:

```bash
XREAD BLOCK <milliseconds> STREAMS <key> <id>
```

Here's how it works:
- The client sends the command and specifies a timeout in milliseconds.
- If the list of entries is empty, the command blocks and waits.
- If a new entry is added to the stream before the timeout expires, the command unblocks and returns the new entry (or entries).
- If the timeout expires and no new data has arrived, the server responds with a null array (`*-1\r\n`).

For example, consider two separate clients. The first client sends a blocking `XREAD` command:

```bash
$ redis-cli XREAD BLOCK 1000 streams some_key 1526985054069-0
```

This client will now block and wait for a new entry to be added.

Meanwhile, a second client can add an entry to the stream:

```bash
$ other-redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
```

If the entry is added within `1000` milliseconds, the first client's `XREAD` command will immediately unblock and respond with the new entry:

```bash
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
      2) 1) temperature
         2) 37
         3) humidity
         4) 94
```

However, if no new entry is added before the timeout expires, the command will fail and return a null array:

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

Next, it will send an `XREAD` command to your server with the `BLOCK` option.

```bash
$ redis-cli XREAD BLOCK 1000 streams stream_key 0-1
```

In another instance of the redis-cli, the tester will add an entry within `500` milliseconds.

```bash
$ redis-cli XADD stream_key 0-2 temperature 95
```

Your server should respond to the first client with the following, encoded as a RESP array:

```json
[
  [
    "stream_key",
    [
      [
        "0-2",
        [
          "temperature",
          "95"
        ]
      ]
    ]
  ]
]
```

Next, the tester will send another blocking `XREAD` command, but this time, will allow the full timeout duration to elapse before checking the response.

```bash
$ redis-cli XREAD block 1000 streams stream_key 0-2
```

In this case, your server should respond with a [null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays) (`*-1\r\n`).
