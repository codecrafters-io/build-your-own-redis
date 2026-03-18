In this stage, you'll implement the default value of `appendfsync` configuration.

### The `appendfsync` option

The `appendfsync` option dictates when a command is written to the append-only file. Its possible values are:

- `always` -> The sent command is written to the append-only file immediately after it is processed,  before the reply is sent to the client.

- `everysec` (default) -> The sent command is written to the append-only file every second.

- `never` -> The sent command is never written to the append-only file.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send the following commands:

```bash
$ redis-cli CONFIG GET appendfsync
# Expected: ["appendfsync", "everysec"]
```

Your server must respond to each `CONFIG GET` command with a RESP array containing two elements: the parameter name and its value, each encoded as a [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

The expected response for `CONFIG GET appendfsync` is:

```
*2\r\n$11\r\nappendfsync\r\n$8\r\neverysec\r\n
```

### Notes

- You don't need to set up anything related to writing to append-only file in this stage. You'll only need to set up the default value of `appendfsync` option.