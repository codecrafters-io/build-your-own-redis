In this stage, you'll accept the value of `appendfsync` from the command line argument.

### The `appendfsync` flag

When the `appendfsync` flag is provided, the provided value overrides the default value `everysec` of the `appendfsync` option. 

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --appendfsync always
```

It will then send the following commands:

```bash
$ redis-cli CONFIG GET appendfsync
```

Your server must respond to each `CONFIG GET` command with a RESP array containing two elements: the parameter name and its value, each encoded as a [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings). The value must be the one passed via the corresponding command-line flag (or the default if the flag was omitted).

If the program is started with `--appendfsync`, then the expected response for `CONFIG GET appendfsync` is:

```
*2\r\n$11\r\nappendfsync\r\n$6\r\nalways\r\n
```

### Notes

- You do not need to implement any AOF persistence logic in this stage. Only parse the flags and return the values from `CONFIG GET`.