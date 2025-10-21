In this stage, you'll extend `XADD` to support auto-generating entry IDs.

### Specifying Entry IDs in `XADD` (Recap)

As a recap, the `XADD` command accepts IDs in three formats:

- Explicit (`1526919030473-0`) (Handled in an earlier stage)
- Auto-generate only the sequence number (`1526919030474-*`) (Handled in the previous stage)
- Auto-generate the time part and sequence number (`*`)

For this stage, you'll handle the third case, where the entire entry ID is auto-generated.

### Auto-Generating Entry IDs

When `*` is used with the `XADD` command, the server automatically generates a unique ID for the new entry:

- It uses the current Unix time in milliseconds for the time part and `0` for the sequence number.
- If an entry with the same timestamp already exists in the stream, the server increments the sequence number by `1`.

Here's an example:

```bash
> XADD stream_key * foo bar
"1526919030474-0"
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then create an entry with `*` as the ID.

```bash
$ redis-cli XADD stream_key * foo bar
```

Your server should respond with a string like `$15\r\n1526919030474-0\r\n`, which is `1526919030474-0` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- The time part of the ID should be the current Unix time in **milliseconds**, not seconds.
- The tester doesn't test the case where a time part already exists in the stream and the sequence number is incremented. This is difficult to test reliably since we'd need to send two commands within the same millisecond.
