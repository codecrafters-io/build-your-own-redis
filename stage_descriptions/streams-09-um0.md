In this stage, you'll add support to querying a stream using the `XREAD` command.

### The XREAD command

[XREAD](https://redis.io/commands/xread/) is used to read data from one or more streams, starting from a specified entry ID.

Here's how it works.

Let's use the entries previously shown as an example.

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

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"
$ redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
$ redis-cli XREAD streams some_key 1526985054069-0
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
         2) 1) temperature
            2) 37
            3) humidity
            4) 94
```

`XREAD` is somewhat similar to `XRANGE`. The primary difference is that `XREAD` only takes a single argument and not a start-end pair.

Another difference is that `XREAD` is exclusive. This means that only entries with the ID greater than what was provided will be included in the response.

Another difference is the return type. `XREAD` returns an array where each element is an array composed of two elements, which are the ID and the list of fields and values.

Here's what the response in the example above actually looks like:

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

When encoded as RESP, it looks like this:

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

The lines are separated into new lines for readability. The return value is just one line.

`XREAD` supports other optional arguments, but we won't deal with them right now.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

First, an entry will be added.

```bash
$ redis-cli XADD stream_key 0-1 temperature 96
```

It'll then send an `XREAD` command to your server.

```bash
$ redis-cli XREAD streams stream_key 0-0
```

Your server should respond with the following:

```text
*1\r\n
*2\r\n
$10\r\nstream_key\r\n
*1\r\n
*2\r\n
$3\r\n0-1\r\n
*2\r\n
$11\r\ntemperature\r\n
$2\r\n96\r\n
```

This is the RESP encoded representation of the following.

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

### Notes
- In the response, the items are separated onto new lines for readability. The tester expects all of these to be in one line.
