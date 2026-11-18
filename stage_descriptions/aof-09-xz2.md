Your server records writes for recovery. In this stage, you'll restore a saved
value at startup by replaying a single command from the AOF before serving
client requests.

### Replaying the append-only file

In earlier stages, you wrote commands to the AOF file as they came in. Now
you'll do the reverse: on startup, read those commands back and replay them to
rebuild the database.

With `--appendonly yes`, the existing manifest at
`<dir>/<append_dir_name>/<append_file_name>.manifest` identifies the file to
replay through its `type i` entry. The commands in that file use the same RESP
encoding as client requests. Applying a saved command must have the same effect
on the database as processing it from a client.

Preserve existing persistence files at startup. Replaying a command must not
append a duplicate of it to the log.

After replaying, the database should be in the same state it was in when the
commands were originally written. This is how Redis recovers data after a
restart.

### Tests

The tester will create a directory `<dir>/<append_dir_name>` containing:

- An AOF file `<random_file_name>.1.incr.aof` with a single RESP-encoded `SET`
  command
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

- The value returned by `GET` matches the value from the `SET` command in the
  AOF file
- Your server used the AOF file named in the manifest

### Notes

- You must read the manifest to find which file to replay. The tester
  intentionally uses a non-default filename (`<random_file_name>.1.incr.aof`
  instead of `<append_file_name>.1.incr.aof`) to make sure your server follows
  the manifest.
- The AOF file contains the same RESP format you already parse from client
  connections. You can reuse your existing RESP parser here.
