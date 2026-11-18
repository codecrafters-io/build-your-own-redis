Your server can log one write. In this stage, you'll retain successive writes in
the AOF, so recovery can reproduce all their changes in order.

### Appending multiple commands

Append the write commands your server supports in the order it processes them.
Later writes must preserve the earlier bytes in the file. This stage exercises
`SET`. You don't need to add commands from other extensions to attempt it.

For example, if the server receives:

```bash
$ redis-cli SET foo 100
$ redis-cli SET bar 200
```

The AOF file should contain both commands in order:

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

_(The `\r\n` sequences above are shown on separate lines for readability. In the
actual file, each command is a continuous sequence of bytes.)_

Each command is appended immediately after the previous one with no separators
between them. On replay, the RESP framing (`*3\r\n...`) is enough to tell where
one command ends and the next begins.

### Tests

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

Continue synchronizing each write before replying when `appendfsync` is
`always`.

### Notes

- If your implementation from earlier stages already appends (rather than
  overwrites), this stage may already pass without changes.
- Continue using the active file named in the manifest.
- You don't need to handle `--appendfsync everysec` in this stage.
