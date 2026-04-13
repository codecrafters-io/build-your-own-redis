In this stage, you'll extend AOF logging to handle multiple write commands.

### Appending Multiple Commands

Each write command should be appended to the AOF file in the order it was received, so the file builds up a complete log of all modifications. This applies to any command that modifies data (like `SET`, `DEL`, `INCR`, `LPUSH`, etc.), not just `SET`.

For example, if the server receives:

```bash
$ redis-cli SET foo 100
$ redis-cli SET bar 200
```

The append-only file should contain both commands in order:

```
*3\r\n
$3\r\n
SET\r\n
$3\r\n
foo\r\n
$3\r\n
100\r\n
*3\r\n
$3\r\n
SET\r\n
$3\r\n
bar\r\n
$3\r\n
200\r\n
```

*(The `\r\n` sequences above are shown on separate lines for readability. In the actual file, each command is a continuous sequence of bytes with `\r\n` as delimiters.)*

Each command is appended immediately after the previous one with no separators between them. On replay, the RESP framing (`*3\r\n...`) is enough to tell where one command ends and the next begins.

### Tests

The tester will create the following under `<dir>/<append_dir_name>`:

- A manifest whose `type i` entry names a file `<random_file_name>.1.incr.aof`
- The corresponding empty AOF file: `<random_file_name>.1.incr.aof`

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will then send two write commands with different keys:

```bash
$ redis-cli SET <key1> <value1>
$ redis-cli SET <key2> <value2>
```

The tester will verify that:

- Both commands appear in the append-only file in the order they were sent
- Each command is encoded in a valid RESP format
- The writes are flushed to disk before the client receives a response (since `appendfsync` is `always`)

### Notes

- If your implementation from earlier stages already appends (rather than overwrites), this stage may already pass without changes.
- You must read the manifest to determine which file to write to. The tester uses a non-default filename.
- You don't need to handle `--appendfsync everysec` in this stage.
