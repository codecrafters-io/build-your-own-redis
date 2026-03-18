In this stage, you'll add support for writing multiple modifying commands to the append-only file.

### Writing to the append-only file

As Redis writes to the append-only file, if it crashes at any point during its operation, it can restore the database using the append-only file.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will check the following:

- The append-only directory is created with the manifest and the append-only file.
- The manifest contains the correct entry for the append-only file.

It will then send several commands over the connection, including multiple `SET` commands for different keys and at least one non-modifying command (for example `GET` or `PING`).

The tester will expect all the write commands to be appended to the append-only file in the order they were sent.

The tester will kill your program, then start it again with the same arguments.

```bash
$ kill -9 <pid_of_your_program>
$ ./your_program.sh
```

On startup, your program should read the manifest, replay the incremental file, and restore keys from the replayed commands.

The tester will then send `GET` commands for the keys that were set, and expect the values to be loaded from the replayed commands.

### Notes

- The tester will only use the `SET` command as the write command.
