In this stage, you'll extend your `XADD` command implementation to support auto-generating entry IDs.

### Specifying entry IDs in XADD (Continued...)

As a recap, there are multiple formats in which the ID can be specified in the `XADD` command:

- Explicit ("1526919030474-0") (Previous stages)
- Auto-generate only sequence number ("1526919030473-*") (Previous stages)
- Auto-generate time part and sequence number ("*") (**This stage**)

We'll now handle the third case.

When `*` is used with the `XADD` command, Redis auto-generates a unique auto-incrementing ID for the message being appended to the stream.

Redis defaults to using the current unix time in milliseconds for the time part and 0 for the sequence number. If the
time already exists in the stream, the sequence number for that record incremented by one will be used.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then create an entry with `*` as the ID.

```bash
$ redis-cli XADD stream_key * foo bar
```

Your server should respond with a string like `$15\r\n1526919030474-0\r\n`, which is `1526919030474-0` encoded as a RESP bulk string.

### Notes

- The time part of the ID should be the current unix time in **milliseconds**, not seconds.
- The tester doesn't test the case where a time part already exists in the stream and the sequence
  number is incremented. This is difficult to test reliably since we'd need to send 2 commands within the same millisecond.
