Your server logs successive writes. In this stage, you'll keep read-only
commands out of the AOF, since replay only needs operations that changed the
database.

### Filtering non-modifying commands

Commands that don't modify data, such as `PING`, `GET`, `ECHO`, and
`CONFIG GET`, shouldn't be written to the AOF. Continue sending their normal
responses to clients.

If the server receives a mix of read and write commands, only the write commands
should appear in the AOF file in the order they were received.

For example, if a client sends:

```bash
$ redis-cli SET foo 1
$ redis-cli GET foo
$ redis-cli PING
$ redis-cli ECHO hello
$ redis-cli SET bar 2
```

The AOF file should contain only the two `SET` commands:

```
*3\r\n
$3\r\n
SET\r\n
$3\r\n
foo\r\n
$1\r\n
1\r\n
*3\r\n
$3\r\n
SET\r\n
$3\r\n
bar\r\n
$1\r\n
2\r\n
```

_(The `\r\n` sequences above are shown on separate lines for readability. In the
actual file, each command is a continuous sequence of bytes.)_

No trace of `GET`, `PING`, or `ECHO` should appear in the file.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will send `SET` commands with `GET` and `ECHO` commands between them.

The tester will verify that:

- The append-only file contains only the write commands, in order
- No bytes from non-modifying commands appear in the file
- Each logged command is in valid RESP format

### Notes

- If you already have a way to distinguish write commands from read commands
  (e.g., for replication), you can reuse that logic here.
- Continue using the active file named in the manifest.
- You don't need to handle `--appendfsync everysec` in this stage.
