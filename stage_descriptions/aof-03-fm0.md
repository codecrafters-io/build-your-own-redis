In this stage, you'll create the append-only directory when AOF persistence is enabled.

### Creating the Append-Only Directory

Now that your server can accept AOF configuration from command-line flags, it's time to start acting on them. When the server starts with `--appendonly yes`, it needs a directory to store its AOF files in. This directory lives inside the `dir` path and is named according to the `appenddirname` option.

For example, if the server is started with:

```bash
$ ./your_program.sh --dir /tmp/redis-data --appendonly yes --appenddirname appendonlydir
```

It should create the directory `/tmp/redis-data/appendonlydir/` if it doesn't already exist.

If `--appendonly` is not set to `yes`, the directory should not be created.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

The tester will verify that:

- The directory `<dir>/<append_dir_name>` has been created
- The directory exists before any client commands are sent (it should be created at startup)

### Notes

- You don't need to create any files inside the directory yet. That comes in later stages.
- If the directory already exists, your server should not return an error.
