In this stage, you’ll extend support for `XREAD` to handle `$` as the starting ID in a blocking read.

### Understanding `$` as an Entry ID

When `$` is passed as the ID, `XREAD` will only return new entries added after the command is sent. This is similar to passing in the maximum ID we currently have in the stream.

For example, suppose we have two separate clients. The first client sends a blocking `XREAD` command with `1000` as the timeout and `$` as the ID:

```bash
$ redis-cli XREAD BLOCK 1000 streams some_key $
```

Then, the second client adds an entry to the `some_key` stream.

```bash
$ other-redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
```

Similar to the behavior detailed in the earlier stages, if the command is sent within `1000` milliseconds, the first client will be unblocked and get the new entry:

```bash
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
         2) 1) temperature
            2) 37
            3) humidity
            4) 94
```

If not, the response will be a [null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays).

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

Next, it will send an `XREAD` command to your server with the `BLOCK` command with `0` as the time and `$` as the ID.

```bash
$ redis-cli XREAD block 0 streams stream_key $
```

In another instance of the redis-cli, the tester will add another entry after `500` milliseconds.

```bash
$ redis-cli XADD stream_key 0-2 temperature 95
```

Your server should respond with the following, as a RESP array:

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

After that, the tester will send another `XREAD` command to your server with the `BLOCK` command, but this time, it'll wait for `1000` milliseconds before checking the response of your server.

```bash
$ redis-cli XREAD block 1000 streams stream_key $
```

Your server should respond with a null array (`*-1\r\n`).
