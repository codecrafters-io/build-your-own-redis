In this stage, you'll extend your `XADD` command implementation to support auto-generating the sequence number of an entry ID.

### Auto-Generating Sequence Numbers

As a recap, the `XADD` command accepts IDs in three formats:

- Explicit (`1526919030473-0`) (Handled in previous stage)
- Auto-generate only sequence number (`1526919030474-*`)
- Auto-generate time part and sequence number (`*`)

For this stage, you'll handle the second case, where only the sequence number is auto-generated.

Redis automatically assigns sequence numbers based on the following conditions:

- If the stream is empty for a given time part, the sequence number starts at `0`.
- If there are already entries with the same time part, the new sequence number is the last sequence number plus `1`.
- The only exception is when the time part is `0`. In that case, the default sequence number starts at `1`.

Here's an example of adding an entry with `*` as the sequence number:

```bash
$ redis-cli XADD some_key "1-*" foo bar
"1-0" # The sequence number is 0 if no prior entries exist

$ redis-cli XADD some_key "1-*" bar baz
"1-1" # Adding another entry will increment the sequence number
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `XADD` command with `*` as the sequence number.

```bash
$ redis-cli XADD stream_key 0-* foo bar
```

Your server should respond with `$3\r\n0-1\r\n`, which is `0-1` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

Next, it will send another `XADD` command with `*` as the sequence number, but this time with a random number as the time part.

```bash
$ redis-cli XADD stream_key 5-* foo bar
```

Your server should respond with `$3\r\n5-0\r\n`, which is `5-0` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

After that, the tester will send the same command again.

```bash
$ redis-cli XADD stream_key 5-* bar baz
```

Your server should respond with `$3\r\n5-1\r\n`, which is `5-1` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- The tester will use a random number for the time part (we use `5` in the example above).
