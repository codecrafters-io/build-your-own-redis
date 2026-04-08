In this stage, you'll create the append-only file when AOF persistence is enabled.

### Creating the Append-Only File

In earlier stages, you created the append-only directory at startup. Now you'll also create an empty AOF file inside that directory.

The file follows a specific naming convention: `<appendfilename>.1.incr.aof`. The `.1.incr` suffix indicates this is the first incremental AOF file. Redis uses incremental files to record commands as they come in, which it can replay later to restore state after a restart.

For example, if the server is started with:

```bash
$ ./your_program.sh --dir /tmp/redis-data --appendonly yes --appenddirname appendonlydir --appendfilename appendonly.aof
```

It should create an empty file at `/tmp/redis-data/appendonlydir/appendonly.aof.1.incr.aof`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

The tester will verify that:

- The directory `<dir>/<append_dir_name>` exists
- An empty file `<dir>/<append_dir_name>/<append_file_name>.1.incr.aof` has been created

### Notes

- The file should be created at startup, not on the first write command.
- You don't need to write any content to the file yet. That comes in later stages.
