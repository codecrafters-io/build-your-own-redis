In this stage, you'll add support for writing a single modifying command to the append-only file after it is processed.

### Writing to the append-only file

When `--appendonly yes` is set, the manifest at `dir`/`appenddirname`/`appendfilename.manifest` lists the append-only file (type `i`) where commands must be stored. After the server executes a write command (for example `SET`), it must append that command to the append-only file in [RESP](https://redis.io/docs/latest/develop/reference/protocol-spec/) form (the same encoding the client would send).

Respect `appendfsync`: when it is `always`, flush the append to disk before sending the reply to the client.

### Tests

The tester will create the following under `dir`/`appenddirname`:

- A manifest whose type `i` entry names a file `<random_file_name>.1.incr.aof`
- The append-only file mentioned in the manifest file: `<random_file_name>.1.incr.aof`.

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will then send a single modifying command, for example:

```bash
$ redis-cli SET <key> <value>
```

The tester will expect the command to be written to the append-only file that was mentioned in the manifest file.

For example, if the command is `SET foo 100`, the content that should be written to the append-only file is:

```
*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\n100\r\n
```

### Notes

- You must read the manifest to determine which file to open for writes. The tester uses a non-default filename to ensure you follow the manifest and not only `<appendfilename>.1.incr.aof`.

- You don't need to implement the behavior of `--appendfsync everysec`. 