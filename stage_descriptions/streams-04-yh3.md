In this stage, you'll extend your `XADD` command implementation to support auto-generating the
sequence number part of the entry ID.

### Specifying entry IDs in XADD

As a recap, there are multiple formats in which the ID can be specified in the `XADD` command:

- Explicit ("1526919030473-0") (Previous stage)
- Auto-generate only sequence number ("1526919030474-*") (**This stage**)
- Auto-generate time part and sequence number ("*") (Next stage)

We dealt with explicit IDs in the last stage. We'll handle the second case in this stage.

When `*` is used for the sequence number, Redis picks the last sequence number used in the
stream (for the same time part) and increments it by 1.

The default sequence number is 0. The only exception is when the time part is also 0. In that case, the default sequence number is 1.

Here's an example of adding an entry with `*` as the sequence number:

```bash
$ redis-cli XADD some_key "1-*" foo bar
"1-0" # If there are no entries, the sequence number will be 0
$ redis-cli XADD some_key "1-*" bar baz
"1-1" # Adding another entry will increment the sequence number
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll send an `XADD` command with `*` as the sequence number.

```bash
$ redis-cli XADD stream_key 0-* foo bar
```

Your server should respond with `$3\r\n0-1\r\n`, which is `0-1` encoded as a RESP bulk string.

It'll then send another `XADD` command with `*` as the sequence number, but this time with a
random number as the time part.

```bash
$ redis-cli XADD stream_key 5-* foo bar
```

Your server should respond with `$3\r\n5-0\r\n`, which is `5-0` encoded as a [RESP bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings)

It'll send the same command again.

```bash
$ redis-cli XADD stream_key 5-* bar baz
```

Your server should respond with `$3\r\n5-1\r\n`, which is `5-1` encoded as a [RESP bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings)

### Notes

- The tester will use a random number for the time part (we use `5` in the example above).
