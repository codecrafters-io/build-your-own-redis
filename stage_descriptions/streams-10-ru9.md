In this stage, you'll add extend support to `XREAD` to allow querying multiple streams.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

First, an entry will be added to a couple of streams.

```bash
$ redis-cli XADD stream_key 0-1 temperature 95
$ redis-cli XADD other_stream_key 0-2 humidity 97
```

It'll then send an `XREAD` command to your server with multiple streams.

```bash
$ redis-cli XREAD streams stream_key other_stream_key 0-0 0-1
```

Your server should respond with the following:

```text
*2\r\n
*2\r\n
$10\r\nstream_key\r\n
*1\r\n
*2\r\n
$3\r\n0-1\r\n
*2\r\n
$11\r\ntemperature\r\n
$2\r\n95\r\n
*2\r\n
$16\r\nother_stream_key\r\n
*1\r\n
*2\r\n
$3\r\n0-2\r\n
*2\r\n
$8\r\nhumidity\r\n
$2\r\n97\r\n
```

This is the RESP encoded representation of the following.

```json
[
  [
    "stream_key",
    [
      [
        "0-1",
        [
          "temperature",
          "95"
        ]
      ]
    ]
  ],
  [
    "other_stream_key",
    [
      [
        "0-2",
        [
          "humidity",
          "97"
        ]
      ]
    ]
  ]
]
```

### Notes
- In the response, the items are separated onto new lines for readability. The tester expects all of these to be in one line.
