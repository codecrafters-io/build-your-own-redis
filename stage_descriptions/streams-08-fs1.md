In this stage, you'll extend support for `XRANGE` to allow querying using `+`.

### Using `XRANGE` with `+`

In the `XRANGE` command, the `end` argument can be specified as `+` to retrieve entries from the given `start` ID to the end of the stream.

Here's an example of how that works:

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"

$ redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"

$ redis-cli XRANGE some_key 1526985054069 +
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

In the example above, `XRANGE` retrieves all the entries from `some_key` starting from the entry with ID `1526985054069-0` to the very end of the stream.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then create a few entries.

```bash
$ redis-cli XADD stream_key 0-1 foo bar
$ redis-cli XADD stream_key 0-2 bar baz
$ redis-cli XADD stream_key 0-3 baz foo
```

Next, it will send an `XRANGE` command using `+` as the `end` ID:

```bash
$ redis-cli XRANGE stream_key 0-2 +
```

Your server should respond with a RESP array containing the entries from the provided `start` ID to the end of the stream.

From the example above, your response should look like the following, encoded as a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays):

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

The raw RESP encoding looks like this:

```text
*2\r\n
*2\r\n
$3\r\n0-2\r\n
*2\r\n
$3\r\nbar\r\n
$3\r\nbaz\r\n
*2\r\n
$3\r\n0-3\r\n
*2\r\n
$3\r\nbaz\r\n
$3\r\nfoo\r\n
```

### Notes
- In the response, the items are shown in separate lines for readability. The tester expects all of these to be in one line.
