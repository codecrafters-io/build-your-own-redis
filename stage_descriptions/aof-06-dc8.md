Your persistence files are ready but empty. In this stage, you'll append one
write command to the AOF so a later restart can recover its effect.

### Writing to the append-only file

So far, you've created the AOF directory, file, and manifest at startup. Now
you'll start using them. When `--appendonly yes` is set and your server
processes a write command (like `SET`), it should append that command to the
append-only file in
[RESP](https://redis.io/docs/latest/develop/reference/protocol-spec/) format.

Your server should read the manifest to find the name of the AOF file to write
to. The manifest's `type i` entry tells you which file is the active incremental
file.

For example, if your server receives:

```bash
$ redis-cli SET foo 100
```

It should append the following to the AOF file:

```
*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\n100\r\n
```

This is the same RESP encoding your server already uses for communication. The
command is stored exactly as it was received, so it can be replayed on restart
to rebuild the state.

### The `appendfsync always` option

Writing a file can leave its bytes buffered in memory by the operating system.
With `appendfsync always`, synchronize the write to storage before replying to
the client. This ordering prevents an acknowledgment from getting ahead of the
requested synchronization. It doesn't guarantee recovery from storage failures.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will then send a single write command:

```bash
$ redis-cli SET <key> <value>
```

After receiving the response, the tester reads the AOF and checks that it
contains the command in valid RESP format. Keep the synchronization-before-reply
requirement even though inspecting the file alone doesn't prove crash
durability.

### Notes

- The manifest determines the active filename. Honor its `type i` entry even
  when it differs from `<appendfilename>.1.incr.aof`.
- You don't need to handle `--appendfsync everysec` in this stage. Only `always`
  is tested.
