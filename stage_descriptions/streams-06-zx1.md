In this stage, you'll add support for querying data from a stream using the `XRANGE` command.

### The XRANGE command

The [XRANGE](https://redis.io/commands/xrange/) command retrieves a range of entries from a stream.

It takes two arguments: `start` and `end`. Both are entry IDs. The command returns all entries in the
stream with IDs between the `start` and `end` IDs. This range is "inclusive", which means that the response
will includes entries with IDs that are equal to the `start` and `end` IDs.

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

The sequence number doesn't need to be included in the start and end IDs provided to the command. If not provided,
XRANGE defaults to a sequence number of 0 for the start and the maximum sequence number for the end.

The return value of the command is not exactly what is shown in the example above. This is already formatted by redis-cli.

The actual return value is a [RESP Array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays) of arrays.

- Each inner array represents an entry.
- The first item in the inner array is the ID of the entry.
- The second item is a list of key value pairs, where the key value pairs are represented as a list of strings.
  - The key value pairs are in the order they were added to the entry.

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

When encoded as a RESP list, it looks like this:

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

In the code block above, the response is separated into multiple lines for readability. The actual
return value doesn't contain any additional newlines.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

First, it'll add a few entries.

```bash
$ redis-cli XADD stream_key 0-1 foo bar
"0-1"
$ redis-cli XADD stream_key 0-2 bar baz
"0-2"
$ redis-cli XADD stream_key 0-3 baz foo
"0-3"
```

Then, it'll send an `XRANGE` command to your server.

```bash
$ redis-cli XRANGE stream_key 0-2 0-3
```

Your server should respond with the following (encoded as a RESP Array):

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
