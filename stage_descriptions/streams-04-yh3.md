Your server validates explicit IDs. In this stage, you'll let clients supply the
time part and leave the sequence number to the server with
`<millisecondsTime>-*`, so several entries can share a timestamp without
duplicate IDs.

### Entry ID progress

As a recap, the `XADD` command accepts IDs in three formats:

- Explicit (`1526919030473-0`) (Handled in previous stages)
- Auto-generate only the sequence number (`1526919030474-*`)
- Auto-generate the time part and sequence number (`*`)

For this stage, you'll handle the second case, where only the sequence number is
auto-generated.

### Auto-generating sequence numbers

Redis automatically assigns sequence numbers based on the following conditions:

- If the stream is empty or the supplied time part is greater than the last
  entry's time part, the sequence number starts at `0`.
- If there are already entries with the same time part, the new sequence number
  is the last sequence number plus `1`.
- The only exception is when the time part is `0`. In that case, the default
  sequence number starts at `1`.

The generated ID must still pass the earlier ordering rules. A supplied time
part smaller than the last entry's time part remains invalid.

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

Your server should respond with `$3\r\n0-1\r\n`, which is `0-1` encoded as a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

Next, it will send another `XADD` command with `*` as the sequence number and a
positive time part. For example:

```bash
$ redis-cli XADD stream_key 5-* foo bar
```

Your server should respond with `$3\r\n5-0\r\n`, which is `5-0` encoded as a
bulk string.

After that, the tester will send the same command again.

```bash
$ redis-cli XADD stream_key 5-* bar baz
```

Your server should respond with `$3\r\n5-1\r\n`, which is `5-1` encoded as a
bulk string.

### Notes

- Use the supplied time part. The value `5` above is an example.
