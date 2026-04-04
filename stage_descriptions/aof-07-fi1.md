In this stage, you'll add support for writing multiple commands to the append-only file.

### Writing to the append-only file

When `--appendonly yes` is set, the manifest at `dir/appenddirname/appendfilename.manifest` lists the append-only file (type `i`) where commands must be stored.

After the server executes a write command, it appends that command to the append-only file in [RESP](https://redis.io/docs/latest/develop/reference/protocol-spec/) form. When several write commands are executed one after another, each should be appended so the file contains all of them, in order.

If the following commands are sent to the server:

```bash
$ redis-cli SET foo 100
$ redis-cli SET bar 200
```

the append-only file contains both commands, in order, as RESP-encoded data—for example:

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

If `appendfsync` is `always`, each command is written to the append-only file before sending the response back to the client.

### Tests

The tester will create the following under `dir/appenddirname`:

- A manifest whose type `i` entry names a file `<random_file_name>.1.incr.aof`
- The append-only file mentioned in the manifest file: `<random_file_name>.1.incr.aof`.

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will then send two modifying commands in order. For example, it may send `SET <key1> <value1>` and then `SET <key2> <value2>` for two different keys. Both commands should appear in the append-only file, in the same order, as RESP-encoded commands.

For example, if the tester sent:

```bash
$ redis-cli SET foo 100
$ redis-cli SET bar 200
```

The content of the append-only file should be:

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

### Notes

- You must read the manifest to determine which file to open for writes. The tester uses a non-default filename to ensure you follow the manifest and not only `<appendfilename>.1.incr.aof`.

- You don't need to implement the behavior of `--appendfsync everysec`.
