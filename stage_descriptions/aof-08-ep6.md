In this stage, you'll add support for writing only the modifying commands to the append-only file.

### Filtering commands

When `--appendonly yes` is set, the server appends only commands that change the dataset. Commands that only read data or perform housekeeping must not be written to the append-only file. Examples of commands that must not be appended include `PING`, `GET`, `CONFIG GET`, and similar non-modifying commands. Examples of commands that must be appended include `SET`, `LPUSH`, `BLPOP` (after any rewriting rules from earlier stages), and other commands that mutate keys or data.

If a client sends a mix of read and write commands, the append-only file should contain only the write commands, in order, each encoded as RESP after parsing and any required transformation.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will then send several commands, including non-modifying commands such as `PING`, `GET` on a key (whether or not the key exists), and `CONFIG GET` for an arbitrary option, interleaved with at least one modifying command (for example `SET <key> <value>`).

For example, if the client sends these commands in order:

```bash
$ redis-cli SET foo 1
$ redis-cli GET foo
$ redis-cli PING
$ redis-cli ECHO hello
$ redis-cli SET bar 2
```

Only the two `SET` commands change the dataset. Nothing from `GET`, `PING`, or `ECHO` is written to the append-only file. The file ends up containing exactly two RESP-encoded commands, in order, back-to-back—the first `SET`, then the second `SET`:

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

The tester will inspect the append-only file and verify that it contains only the RESP-encoded modifying command(s), in the correct order. None of the bytes for `PING`, `GET`, `ECHO`, or `CONFIG GET` should appear in the file.

### Notes

- You must read the manifest to determine which file to open for writes. The tester uses a non-default filename to ensure you follow the manifest and not only `<appendfilename>.1.incr.aof`.

- You don't need to implement the behavior of `--appendfsync everysec`.
