In this stage, you'll extend support for `XRANGE` to allow querying using `+`.

### Using XRANGE with +

In the previous stage, we saw that `XRANGE` takes `start` and `end` as arguments.

In addition to accepting an explicit entry ID, `end` can also be specified as `+`. When `+` is used, `XRANGE` retrieves entries until the end of the stream.

Here's an example of how that works.

We'll use our previous example for entries existing in a stream.

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

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then create a few entries.

```bash
$ redis-cli XADD stream_key 0-1 foo bar
$ redis-cli XADD stream_key 0-2 bar baz
$ redis-cli XADD stream_key 0-3 baz foo
```

It'll then send an `XRANGE` command to your server.

```bash
$ redis-cli XRANGE stream_key 0-2 +
```

Your server should respond with the following:

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

This is the RESP encoded representation of the following.

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

### Notes
- In the response, the items are separated onto new lines for readability. The tester expects all of these to be in one line.
