In this stage, you'll extend support for `XRANGE` to allow querying using `-`.

### Using XRANGE with -

In the previous stage, we saw that `XRANGE` takes `start` and `end` as arguments.

In addition to accepting an explicit entry ID, `start` can also be specified as `-`. When `-` is used, `XRANGE` retrieves entries from the beginning of the stream.

Here's an example of how that works.

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"
$ redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
$ redis-cli XRANGE some_key - 1526985054079
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

In the example above, `XRANGE` retrieves all entries from the beginning of the stream to the entry with ID `1526985054079-0`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then create a few entries.

```bash
$ redis-cli XADD stream_key 0-1 foo bar
"0-1"
$ redis-cli XADD stream_key 0-2 bar baz
"0-2"
$ redis-cli XADD stream_key 0-3 baz foo
"0-3"
```

It'll then send an `XRANGE` command to your server.

```bash
$ redis-cli XRANGE stream_key - 0-2
1) 1) 0-1
   2) 1) foo
      2) bar
2) 1) 0-2
   2) 1) bar
      2) baz
```

Your server should respond with the following, encoded as a [RESP Array](https://redis.io/docs/reference/protocol-spec/#arrays):

```json
[
  [
    "0-1",
    [
      "foo",
      "bar"
    ]
  ],
  [
    "0-2",
    [
      "bar",
      "baz"
    ]
  ]
]
```
