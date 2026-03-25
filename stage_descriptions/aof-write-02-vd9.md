In this stage, you'll accept the values of AOF configuration options from the command line flags.

### AOF Persistence

When the `--dir`, `--appendonly`, `--appenddirname`, `--appendfilename`, and `--appendfsync` flags are provided, these override the default value of the corresponding options.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync everysec
```

It will then send the following commands:

```bash
$ redis-cli CONFIG GET dir
$ redis-cli CONFIG GET appendonly
$ redis-cli CONFIG GET appenddirname
$ redis-cli CONFIG GET appendfilename
$ redis-cli CONFIG GET appendfsync
```

Your server must respond to each `CONFIG GET` command with a RESP array containing two elements: the parameter name and its value, each encoded as a [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings). The value must be the one passed via the corresponding command-line flag (or the default if the flag was omitted).

If the program is started with `--appendonly yes --appenddirname myaofdir --appendfilename myfile.aof`, then the expected response for `CONFIG GET appendonly` is:

```
*2\r\n$9\r\nappendonly\r\n$3\r\nyes\r\n
```

### Notes

- You do not need to implement any AOF persistence logic in this stage. Only parse the flags and return the values from `CONFIG GET`.