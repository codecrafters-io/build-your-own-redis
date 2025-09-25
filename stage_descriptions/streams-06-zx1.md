In this stage, you'll add support for querying data from a stream using the `XRANGE` command.

### The `XRANGE` command

The [`XRANGE`](https://redis.io/docs/latest/commands/xrange/) command retrieves a range of entries from a stream.

It takes two arguments: a `start` ID and an `end` ID, and returns all entries within that range. The range is inclusive, meaning entries with IDs equal to the `start` and `end` IDs are included.

Here's an example of how it works:

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0" # (ID of the first added entry)
$ redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
$ redis-cli XRANGE some_key 1526985054069 1526985054079
1) 1) 1526985054069-0
   2) 1) temperature
      2) 36
      3) humidity
      4) 95
2) 1) 1526985054079-0
   2) 1) temperature
      2) 37
      3) humidity
      4) 94
```

The command can accept IDs in the format `<millisecondsTime>-<sequenceNumber>`, but the sequence number is optional. If you don't provide a sequence number:

- For the `start` ID, the sequence number defaults to `0`.
- For the `end` ID, the sequence number defaults to the maximum sequence number.

The return value of the command is not exactly what is shown in the example above. This is already formatted by redis-cli.

The actual return value is a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays) of arrays.

Each inner array represents a single entry and contains two elements:

- The entry's ID (as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings)).
- An array of key-value pairs (as bulk strings) in the order they were added.

The return value of the example above is actually something like this:

```json
[
  [
    "1526985054069-0",
    [
      "temperature",
      "36",
      "humidity",
      "95"
    ]
  ],
  [
    "1526985054079-0",
    [
      "temperature",
      "37",
      "humidity",
      "94"
    ]
  ],
]
```

When encoded as a RESP array, it looks like this:

```text
*2\r\n
*2\r\n
$15\r\n1526985054069-0\r\n
*4\r\n
$11\r\ntemperature\r\n
$2\r\n36\r\n
$8\r\nhumidity\r\n
$2\r\n95\r\n
*2\r\n
$15\r\n1526985054079-0\r\n
*4\r\n
$11\r\ntemperature\r\n
$2\r\n37\r\n
$8\r\nhumidity\r\n
$2\r\n94\r\n
```
*(This response is separated into multiple lines for readability. The actual return value doesn't contain any additional newlines.)*

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then add a few entries to a stream.

```bash
$ redis-cli XADD stream_key 0-1 foo bar
"0-1"
$ redis-cli XADD stream_key 0-2 bar baz
"0-2"
$ redis-cli XADD stream_key 0-3 baz foo
"0-3"
```

Next, it will send an `XRANGE` command to your server.

```bash
$ redis-cli XRANGE stream_key 0-2 0-3
```

Your server should respond with a RESP array containing the range of entries from the IDs provided. 

Based on the example above, the response should look like the following (encoded as a RESP array):

```json
[
  [
    "0-2",
    [
      "bar",
      "baz"
    ]
  ],
  [
    "0-3",
    [
      "baz",
      "foo"
    ]
  ]
]
```
