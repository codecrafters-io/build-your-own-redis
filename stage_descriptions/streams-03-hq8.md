In this stage, you'll add support for validating entry IDs to the `XADD` command.

### Entry IDs

Here's an example of stream entries from the previous stage:

```yaml
entries:
  - id: 1526985054069-0 # (ID of the first entry)
    temperature: 36 # (A key value pair in the first entry)
    humidity: 95 # (Another key value pair in the first entry)

  - id: 1526985054079-0 # (ID of the second entry)
    temperature: 37 # (A key value pair in the first entry)
    humidity: 94 # (Another key value pair in the first entry)

  # ... (and so on)
```

Entry IDs are always composed of two integers: `<millisecondsTime>-<sequenceNumber>`.

Entry IDs are unique within a stream, and they're guaranteed to be incremental - i.e. an
entry added later will always have an ID greater than an entry added in the past. More
on this in the next section.

### Specifying entry IDs in XADD

There are multiple formats in which the ID can be specified in the XADD command:

- Explicit ("1526919030474-0") (**This stage**)
- Auto-generate only sequence number ("1526919030474-*") (Next stages)
- Auto-generate time part and sequence number ("*") (Next stages)

In this stage, we'll only deal with explicit IDs. We'll add support for the other two cases in the next stages.

Your XADD implementation should validate the ID passed in.

- The ID should be greater than the ID of the last entry in the stream.
  - The `millisecondsTime` part of the ID should be greater than or equal to the `millisecondsTime` of the last entry.
  - If the `millisecondsTime` part of the ID is equal to the `millisecondsTime` of the last entry, the `sequenceNumber` part of the ID should be greater than the `sequenceNumber` of the last entry.
- If the stream is empty, the ID should be greater than `0-0`

Here's an example of adding an entry with a valid ID followed by an invalid ID:

```bash
$ redis-cli XADD some_key 1-1 foo bar
"1-1"
$ redis-cli XADD some_key 1-1 bar baz
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item
```

Here's another such example:

```bash
$ redis-cli XADD some_key 1-1 foo bar
"1-1"
$ redis-cli XADD some_key 0-2 bar baz
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item
```

The minimum entry ID that Redis supports is 0-1. Passing in an ID lower than should result in an error.

```bash
$ redis-cli XADD some_key 0-0 bar baz
(error) ERR The ID specified in XADD must be greater than 0-0
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll create a few entries usind `XADD`.

```bash
$ redis-cli XADD stream_key 1-1 foo bar
"1-1"
$ redis-cli XADD stream_key 1-2 bar baz
"1-2"
```

It'll send another `XADD` command with the same time and sequence number as the last entry.

```bash
$ redis-cli XADD stream_key 1-2 baz foo
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item
```

Your server should respond with "-ERR The ID specified in XADD is equal or smaller than the target stream top item\r\n", which is the error message above encoded as a
[simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors).

The tester will then send another `XADD` command with a smaller value for the time and a larger value for the sequence number.

```bash
$ redis-cli XADD stream_key 0-3 baz foo
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item
```

Your server should also respond with the same error message.

After that, the tester will send another `XADD` command with `0-0` as the ID.

```bash
$ redis-cli XADD stream_key 0-0 baz foo
```

Your server should respond with "-ERR The ID specified in XADD must be greater than 0-0\r\n", which is the error message above encoded as a
[RESP simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors).
