In this stage, you'll accept AOF configuration values from command-line flags.

### Overriding Defaults With Flags

In the previous stage, you set up default values for AOF options. Now you'll allow those defaults to be overridden by command-line flags: `--dir`, `--appendonly`, `--appenddirname`, `--appendfilename`, and `--appendfsync`.

When a flag is provided, its value takes precedence over the default. When a flag is omitted, the default from the previous stage still applies.

### Tests

The tester will execute your program with some or all of the flags:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync everysec
```

It will then query each option using `CONFIG GET`:

```bash
$ redis-cli CONFIG GET dir
$ redis-cli CONFIG GET appendonly
$ redis-cli CONFIG GET appenddirname
$ redis-cli CONFIG GET appendfilename
$ redis-cli CONFIG GET appendfsync
```

Each response should return the value passed via the corresponding flag. For example, if the program is started with `--appendonly yes`, then `CONFIG GET appendonly` should return:

```
*2\r\n$10\r\nappendonly\r\n$3\r\nyes\r\n
```

The tester will verify that:

- Each `CONFIG GET` returns the value from the corresponding command-line flag
- Options without flags still return their default values

### Notes

- You don't need to implement any AOF persistence logic in this stage. Just parse the flags and return the values from `CONFIG GET`.
- If you already parse command-line flags for other options (like `--port`), you can follow the same approach for these new flags.
- You don't need to handle duplicate flags or any specific flag ordering. The tester will pass each flag at most once.
