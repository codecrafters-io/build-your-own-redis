In this stage, you'll add support for validating entry IDs to the `XADD` command.

### Entry IDs

Entry IDs are crucial for maintaining the order of entries in Redis streams.

Each ID is made up of two integers separated by a dash: `<millisecondsTime>-<sequenceNumber>`.

Here's an example from the previous stage:

```yaml
entries:
  - id: 1526985054069-0 # (ID of the first entry)
    temperature: 36
    humidity: 95

  - id: 1526985054079-0 # (ID of the second entry)
    temperature: 37
    humidity: 94

  # ... (and so on)
```

These IDs are unique within a stream and are guaranteed to be incremental. This means a new entry's ID will always be greater than the ID of any previous entry.

### Specifying Entry IDs in `XADD`

There are multiple formats in which the ID can be specified in the `XADD` command:

- Explicit (`1526919030474-0`)
- Auto-generate only sequence number (`1526919030474-*`)
- Auto-generate the time and sequence number (`*`)

For this stage, you will only handle explicit IDs (e.g., `1526919030474-0`). We'll add support for the other two cases in the next stages.

Your `XADD` implementation must validate the provided ID based on the following rules:

- The ID must be strictly greater than the last entry's ID.
  - The `millisecondsTime` portion of the new ID must be greater than or equal to the last entry's `millisecondsTime`.
  - If the `millisecondsTime` values are equal, the `sequenceNumber` of the new ID must be greater than the last entry's `sequenceNumber`.
- If the stream is empty, the ID must be greater than `0-0`. The minimum valid ID Redis accepts is `0-1`.

Here's an example of adding an entry with a valid ID followed by an invalid ID:

```bash
$ redis-cli XADD some_key 1-1 foo bar
"1-1"
$ redis-cli XADD some_key 1-1 bar baz
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item
```

The second command fails because `1-1` is not strictly greater than the last ID.

Here's another example:

```bash
$ redis-cli XADD some_key 1-1 foo bar
"1-1"
$ redis-cli XADD some_key 0-2 bar baz
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item
```

The ID `0-2` is invalid because its `millisecondsTime` is less than the last ID's `millisecondsTime`.

Finally, passing `0-0` is always invalid, since IDs must be strictly greater than `0-0`:

```bash
$ redis-cli XADD some_key 0-0 bar baz
(error) ERR The ID specified in XADD must be greater than 0-0
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then create a few entries using `XADD`.

```bash
$ redis-cli XADD stream_key 1-1 foo bar
"1-1"
$ redis-cli XADD stream_key 1-2 bar baz
"1-2"
```

Next, it will send a few `XADD` commands with an invalid ID, such as `1-2` or `0-3`. 

```bash
# The exact time and sequence number as the last entry
$ redis-cli XADD stream_key 1-2 baz foo
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item

# A smaller value for the time and a larger value for the sequence number
$ redis-cli XADD stream_key 0-3 baz foo
(error) ERR The ID specified in XADD is equal or smaller than the target stream top item
```

In each case, your server should respond with `-ERR The ID specified in XADD is equal or smaller than the target stream top item\r\n\`, encoded as a
[simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors).

After that, the tester will send another `XADD` command with `0-0` as the ID.

```bash
$ redis-cli XADD stream_key 0-0 baz foo
(error) ERR The ID specified in XADD must be greater than 0-0
```

Your server should respond with `-ERR The ID specified in XADD must be greater than 0-0\r\n`, which is the error message above encoded as a
[simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors).
