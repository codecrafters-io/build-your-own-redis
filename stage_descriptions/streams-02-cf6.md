In this stage, you'll add support for creating a [Redis stream](https://redis.io/docs/data-types/streams/) using the `XADD` command.

### Redis Streams & Entries

Streams are one of the data types that Redis supports. A stream is identified by a key, and it contains multiple entries.

Each entry consists of one or more key-value pairs, and is assigned a unique ID.

For example, if you were using a Redis stream to store real-time data from a temperature & humidity monitor, the contents of the stream might look like this:

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

We'll take a closer look at the format of entry IDs (`1526985054069-0` and `1526985054079-0` in the example above) in the upcoming stages.

### The XADD command

The [XADD](https://redis.io/commands/xadd/) command appends an entry to a stream. If a stream doesn't exist already, it is created.

Here's how it works:

```bash
$ redis-cli XADD stream_key 1526919030474-0 temperature 36 humidity 95
"1526919030474-0" # (ID of the entry created)
```

The return value is the ID of the entry created, encoded as a [bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings).

`XADD` supports other optional arguments, but we won't deal with them in this challenge.

`XADD` also supports auto-generating entry IDs. We'll add support for that in later stages. For now, we'll only deal with
explicit IDs (like `1526919030474-0` in the example above).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `XADD` command to your server and expect the ID as a response.

```bash
$ redis-cli XADD stream_key 0-1 foo bar
"0-1"
```

Your server should respond with `$3\r\n0-1\r\n`, which is `0-1` encoded as a [RESP bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings).

The tester will then send a `TYPE` command to your server.

```bash
$ redis-cli TYPE stream_key
"stream"
```

Your server should respond with `+stream\r\n`, which is `stream` encoded as a [RESP simple string](https://redis.io/docs/reference/protocol-spec/#simple-strings).

### Notes

- You still need to handle the "string" and "none" return values for the `TYPE` command. "stream" should only be returned for keys that are streams.
