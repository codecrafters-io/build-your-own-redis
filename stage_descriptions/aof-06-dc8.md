In this stage, you'll write a single modifying command to the append-only file.

### Writing to the Append-Only File

So far, you've created the AOF directory, file, and manifest at startup. Now you'll start using them. When `--appendonly yes` is set and your server processes a write command (like `SET`), it should append that command to the append-only file in [RESP](https://redis.io/docs/latest/develop/reference/protocol-spec/) format.

Your server should read the manifest to find the name of the AOF file to write to. The manifest's `type i` entry tells you which file is the active incremental file.

For example, if your server receives:

```bash
$ redis-cli SET foo 100
```

It should append the following to the AOF file:

```
*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\n100\r\n
```

This is the same RESP encoding your server already uses for communication. The command is stored exactly as it was received, so it can be replayed on restart to rebuild the state.

### The `appendfsync always` Option

When `appendfsync` is set to `always`, the server must flush the write to disk before sending a response to the client. This guarantees that no acknowledged write can be lost, even if the server crashes immediately after responding.

### Tests

The tester will create the following under `<dir>/<append_dir_name>`:

- A manifest whose `type i` entry names a file `<random_file_name>.1.incr.aof`
- The corresponding empty AOF file: `<random_file_name>.1.incr.aof`

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will then send a single write command:

```bash
$ redis-cli SET <key> <value>
```

The tester will verify that:

- The command is appended to the AOF file named in the manifest (not the default `<appendfilename>.1.incr.aof`)
- The command is written in a valid RESP format
- The write is flushed to disk before the client receives a response

### Notes

- You must read the manifest to determine which file to write to. The tester intentionally uses a non-default filename to make sure your server follows the manifest.
- You don't need to handle `--appendfsync everysec` in this stage. Only `always` is tested.
- The RESP encoding of the command is the same format you're already using for client-server communication. You're just writing it to a file instead of a socket.
