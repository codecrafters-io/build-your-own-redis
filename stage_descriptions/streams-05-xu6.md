Your server can choose a sequence number for a client-supplied time. In this
stage, you'll support `XADD` with `*` so the server chooses the time part too.

### Entry ID progress

As a recap, the `XADD` command accepts IDs in three formats:

- Explicit (`1526919030473-0`) (Handled in earlier stages)
- Auto-generate only the sequence number (`1526919030474-*`) (Handled in
  previous stages)
- Auto-generate the time part and sequence number (`*`)

For this stage, you'll handle the third case, where the entire entry ID is
auto-generated.

### Auto-generating entry IDs

When `*` is used with the `XADD` command, the server automatically generates a
unique ID for the new entry:

- If the current Unix time in milliseconds is greater than the last entry's time
  part, use the current time and sequence number `0`.
- Otherwise, keep the last entry's time part and increment its sequence number
  by `1`. This keeps IDs increasing even if the clock moves backward or a client
  previously supplied a time ahead of the clock.

For an empty stream, use the current time and sequence number `0`.

Unix time counts elapsed time since 1 January 1970 at 00:00 UTC. Here the unit
is milliseconds, so the example's time part has thirteen digits.

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
$ redis-cli XADD stream_key '*' foo bar
```

Your server should respond with a string like `$15\r\n1526919030474-0\r\n`,
which is `1526919030474-0` encoded as a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- Convert the clock value to milliseconds before using it as the time part.
- The tester doesn't test the case where a time part already exists in the
  stream and the sequence number is incremented.
