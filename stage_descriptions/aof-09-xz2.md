In this stage, you'll restore the state on startup by replaying commands from the append-only file.

### Replaying the Append-Only File

In earlier stages, you wrote commands to the AOF file as they came in. Now you'll do the reverse: on startup, read those commands back and replay them to rebuild the database.

When the server starts with `--appendonly yes` and the append-only directory already exists, it should:

1. Read the manifest file at `<dir>/<append_dir_name>/<append_file_name>.manifest`
2. Find the entry with `type i` (the incremental file)
3. Open that file and parse the RESP-encoded commands inside it
4. Execute each command as if a client had sent it

After replaying, the database should be in the same state it was in when the commands were originally written. This is how Redis recovers data after a restart.

### Tests

The tester will create a directory `<dir>/<append_dir_name>` containing:

- An AOF file `<random_file_name>.1.incr.aof` with a single RESP-encoded `SET` command
- A manifest file `<append_file_name>.manifest` pointing to that AOF file:

```
file <random_file_name>.1.incr.aof seq 1 type i
```

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

After startup, the tester will send a `GET` command to verify the replayed data:

```bash
$ redis-cli GET <key>
```

The tester will verify that:

- The value returned by `GET` matches the value from the `SET` command in the AOF file
- Your server read the AOF file named in the manifest, not a hardcoded filename

### Notes

- You must read the manifest to find which file to replay. The tester intentionally uses a non-default filename (`<random_file_name>.1.incr.aof` instead of `<append_file_name>.1.incr.aof`) to make sure your server follows the manifest.
- The AOF file contains the same RESP format you already parse from client connections. You can reuse your existing RESP parser here.
