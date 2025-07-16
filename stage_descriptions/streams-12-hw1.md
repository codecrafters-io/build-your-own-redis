In this stage, you'll extend support to `XREAD` to allow for the blocking command not timing out.

### Understanding blocking without timeout

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

On one instance of the redis-cli, we'd add an entry and send a blocking `XREAD` command with 0 as the time passed in.

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"
$ redis-cli XREAD block 0 streams some_key 1526985054069-0
```

Then, on another instance of the redis-cli, we add another entry.

```bash
$ other-redis-cli XADD some_key 1526985054079-0 temperature 37 humidity 94
"1526985054079-0"
```

The difference now is that the first instance of the redis-cli doesn't time out and responds with null no matter how much time passes. It will wait until another entry is added. The return value after an entry is added is similar to the last stage.

```bash
$ redis-cli XADD some_key 1526985054069-0 temperature 36 humidity 95
"1526985054069-0"
$ redis-cli XREAD block 0 streams some_key 1526985054069-0
1) 1) "some_key"
   2) 1) 1) 1526985054079-0
         2) 1) temperature
            2) 37
            3) humidity
            4) 94
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

It'll then send an `XREAD` command to your server with the `BLOCK` command with the time passed in being 0.

```bash
$ redis-cli XREAD block 0 streams stream_key 0-1
```

It'll then wait for 1000 milliseconds before checking if there is a response. Your server should not have a new response. It'll then add another entry.

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
$2\r\n95\r\n
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

### Notes
- In the response, the items are separated onto new lines for readability. The tester expects all of these to be in one line.
