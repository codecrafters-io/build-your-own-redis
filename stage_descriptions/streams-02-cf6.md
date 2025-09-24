In this stage, you'll add support for creating [Redis streams](https://redis.io/docs/latest/develop/data-types/streams/) using the `XADD` command.

### Redis Streams & Entries

A Redis stream is a data type that stores a sequence of entries. Each entry consists of a unique ID and one or more key-value pairs.

Streams are great for applications that need to handle real-time data, like event logs and message queues.

For example, if you were using a Redis stream to store data for a temperature & humidity monitor, the stream might look like this:

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

We’ll take a closer look at how entry IDs (like `1526985054069-0`) are structured in later stages.

### The `XADD` command

The [`XADD`](https://redis.io/commands/xadd/) command appends an entry to a stream. If the stream doesn't exist, it is created automatically.

The `XADD` command accepts a stream key, an entry ID, and one or more key-value pairs as arguments:

```bash
$ redis-cli XADD stream_key 1526919030474-0 temperature 36 humidity 95
"1526919030474-0"
```

The return value is the ID of the newly added entry as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

`XADD` supports other [optional arguments](https://redis.io/docs/latest/commands/xadd/#optional-arguments), but we won't deal with them in this challenge.

`XADD` also supports auto-generated entry IDs, but for this stage, you'll only deal with explicit IDs (like `1526919030474-0`).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `XADD` command to your server and expect the ID as a response. For example, it might send:

```bash
$ redis-cli XADD stream_key 0-1 foo bar
"0-1"
```

In this case, your server should respond with `$3\r\n0-1\r\n`, which is `0-1` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

Next, the tester will send a `TYPE` command to your server to verify the key's type.

```bash
$ redis-cli TYPE stream_key
"stream"
```

Your server should respond with `+stream\r\n`, which is `stream` encoded as a [simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings).

### Notes

- You still need to handle the `string` and `none` return values for the `TYPE` command. `stream` should only be returned for keys that are streams.
