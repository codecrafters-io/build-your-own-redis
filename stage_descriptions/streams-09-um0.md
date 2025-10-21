In this stage, you'll add support to querying a stream using the `XREAD` command.

### The `XREAD` Command

The [`XREAD`](https://redis.io/docs/latest/commands/xread/) command is used to read data from one or more streams, starting from a specified entry ID.

Unlike `XRANGE`, which requires both a `start` and `end` ID, `XREAD` takes only a single ID. 

Another difference is that `XREAD` is exclusive. This means that the command retrieves all entries with an ID greater than the specified ID.

The basic syntax for the command is:
```bash
XREAD STREAMS <key> <id>
```

`XREAD` supports other optional arguments, but we won't deal with them at this stage.

Here's an example of its behavior:

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"

$ redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"

$ redis-cli XREAD STREAMS some_key 1526985054069-0
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
         2) 1) temperature
            2) 37
            3) humidity
            4) 94
```

The return value is an array of streams, where each stream contains:

- The stream key (as a bulk string).
- An array of entries, where each entry is an array of two parts:
  - The entry's ID (as a bulk string).
  - An array of key-value pairs (as bulk strings).

Here's what the response from the example above would look like:

```json
[
  [
    "some_key",
    [
      [
        "1526985054079-0",
        [
          "temperature",
          "37",
          "humidity",
          "94"
        ]
      ]
    ]
  ]
]
```

When encoded as a RESP array, it looks like this:

```text
*1\r\n
*2\r\n
$8\r\nsome_key\r\n
*1\r\n
*2\r\n
$15\r\n1526985054079-0\r\n
*4\r\n
$11\r\ntemperature\r\n
$2\r\n37\r\n
$8\r\nhumidity\r\n
$2\r\n94\r\n
```
(*The result is shown on separate lines for readability. The actual return value is a single line.*)

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `XADD` command to add an entry to a stream.

```bash
$ redis-cli XADD stream_key 0-1 temperature 96
```

Next, the tester will send an `XREAD` command to your server.

```bash
$ redis-cli XREAD STREAMS stream_key 0-0
```

Your server should respond with a RESP array containing the correct stream entries.

From the example above, your response should look like the following, encoded as a RESP array:

```json
[
  [
    "stream_key",
    [
      [
        "0-1",
        [
          "temperature",
          "96"
        ]
      ]
    ]
  ]
]
```
