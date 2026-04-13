In this stage, you'll ensure that only write commands are logged to the append-only file.

### Filtering Non-Modifying Commands

Commands that don't modify data, like `PING`, `GET`, and `ECHO`, should never be written to an AOF file since they don't change any state.

If the server receives a mix of read and write commands, only the write commands should appear in the AOF file in the order they were received.

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

*(The `\r\n` sequences above are shown on separate lines for readability. In the actual file, each command is a continuous sequence of bytes.)*

No trace of `GET`, `PING`, or `ECHO` should appear in the file.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will send a mix of modifying and non-modifying commands, including `PING`, `GET`, `ECHO`, and `CONFIG GET` interleaved with at least one write command.

The tester will verify that:

- The append-only file contains only the write commands, in order
- No bytes from non-modifying commands appear in the file
- Each logged command is in valid RESP format

### Notes

- If you already have a way to distinguish write commands from read commands (e.g., for replication), you can reuse that logic here.
- You must read the manifest to determine which file to write to. The tester uses a non-default filename.
- You don't need to handle `--appendfsync everysec` in this stage.
