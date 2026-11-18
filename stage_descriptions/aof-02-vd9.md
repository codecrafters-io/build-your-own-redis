Your server now reports AOF defaults. In this stage, you'll allow startup flags
to override them so each server can choose its own persistence settings.

### Overriding defaults with flags

In previous stages, you set up default values for AOF options. Now you'll allow
those defaults to be overridden by command-line flags: `--dir`, `--appendonly`,
`--appenddirname`, `--appendfilename`, and `--appendfsync`.

When a flag is provided, its value takes precedence over the default. When a
flag is omitted, the default from previous stages still applies.

### Tests

The tester will execute your program with configuration flags, for example:

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

Each response should return the configured value, using the default for an
omitted flag. For example, if the program is started with `--appendonly yes`,
then `CONFIG GET appendonly` should return:

```
*2\r\n$10\r\nappendonly\r\n$3\r\nyes\r\n
```

### Notes

- You don't need to implement any AOF persistence logic in this stage. Just
  parse the flags and return the values from `CONFIG GET`.
- If you already parse command-line flags for other options (like `--port`), you
  can follow the same approach for these new flags.
- The tester passes each flag at most once. Accept the flags without requiring a
  particular order.
