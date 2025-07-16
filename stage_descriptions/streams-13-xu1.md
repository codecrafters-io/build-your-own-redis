In this stage, you'll extend support to `XREAD` to allow for passing in `$` as the ID for a blocking command.

### Understanding $

Using `$` as the ID passed to a blocking `XREAD` command signals that we only want new entries. This is similar to passing in the maximum ID we currently have in the stream.

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

On one instance of the redis-cli, we'd add an entry and send a blocking `XREAD` command with `1000` as the time passed in and `$` as the id passed in.

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"
$ redis-cli XREAD block 1000 streams some_key $
```

Then, on another instance of the redis-cli, we add another entry.

```bash
$ other-redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
```

Similar to the behavior detailed in the earlier stages, if the command was sent within 1000 milliseconds, the redis-cli will respond with the new entry.

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"
$ redis-cli XREAD block 1000 streams some_key 1526985054069-0
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
         2) 1) temperature
            2) 37
            3) humidity
            4) 94
```

If not, the return type would still be a null representation of a bulk string.

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"
$ redis-cli XREAD block 1000 streams some_key 1526985054069-0
(nil)
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

First, an entry will be added to a stream.

```bash
$ redis-cli XADD stream_key 0-1 temperature 96
```

It'll then send an `XREAD` command to your server with the `BLOCK` command with `0` as the time and `$` as the ID.

```bash
$ redis-cli XREAD block 0 streams stream_key $
```

On another instance of the redis-cli, another entry will be added in 500 milliseconds after sending the `XREAD` command.

```bash
$ redis-cli XADD stream_key 0-2 temperature 95
```

Your server should respond with the following:

```text
*1\r\n
*2\r\n
$10\r\stream_key\r\n
*1\r\n
*2\r\n
$3\r\n0-2\r\n
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
        "0-2",
        [
          "temperature",
          "95"
        ]
      ]
    ]
  ]
]
```

It'll send another `XREAD` command to your server with the `BLOCK` command but this time, it'll wait for 1000 milliseconds before checking the response of your server.

```bash
$ redis-cli XREAD block 1000 streams stream_key $
```

Your server should respond with `$-1\r\n` which is a `null` representation of a RESP bulk string.

### Notes
- In the response, the items are separated onto new lines for readability. The tester expects all of these to be in one line.
