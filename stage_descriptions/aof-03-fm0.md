Your server can report its AOF settings. In this stage, you'll use those
settings to create the directory that will hold its persistence files when AOF
is enabled.

### Creating the append-only directory

When the server starts with `--appendonly yes`, create the directory named by
`appenddirname` inside `dir`.

For example, if the server is started with:

```bash
$ ./your_program.sh --dir /tmp/redis-data --appendonly yes --appenddirname appendonlydir
```

It should create the directory `/tmp/redis-data/appendonlydir/` if it doesn't
already exist.

If `--appendonly` is not set to `yes`, the directory should not be created.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

The tester will verify that:

- The directory `<dir>/<append_dir_name>` has been created
- The directory exists before any client commands are sent (it should be created
  at startup)

### Notes

- You don't need to create any files inside the directory yet. That comes in
  later stages.
- If the directory already exists, your server should not return an error.
