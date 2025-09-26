In this stage, you'll add support for querying multiple streams using the `XREAD` command.

### The `XREAD` Command for Multiple Streams

When reading from multiple streams, the `XREAD` command takes the `STREAMS` keyword, followed by a list of stream keys, and then a corresponding list of entry IDs for each stream.

The command format is: 

```bash
XREAD STREAMS <key1> <key2> ... <id1> <id2> ...
```

The server's response remains a RESP array of streams where:

- Each stream is a two-item array: the stream's key and the entries read from it.
- The order of the streams in the response must match the order in which they were specified in the command.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send multiple `XADD` commands to add entries to several streams.

```bash
$ redis-cli XADD stream_key 0-1 temperature 95
$ redis-cli XADD other_stream_key 0-2 humidity 97
```

Next, the tester will send an `XREAD` command to your server with multiple streams.

```bash
$ redis-cli XREAD streams stream_key other_stream_key 0-0 0-1
```

Your server should respond with a RESP array containing the results for all the specified streams.

From the example above, your response should look like the following, encoded as a RESP array:

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
