In this stage, you'll add support for writing multiple commands to the append-only file.

### Writing to the append-only file

When `--appendonly yes` is set, the manifest at `dir/appenddirname/appendfilename.manifest` lists the append-only file (type `i`) where commands must be stored.

After the server executes a write command, it appends that command to the append-only file in [RESP](https://redis.io/docs/latest/develop/reference/protocol-spec/) form. The command is not directly byte-copied from the client's buffer to the file. The command should be decoded, transformed when required, and re-encoded as RESP before being written to the file.

If the following commands are sent to the server:

```bash
$ redis-cli
> LPUSH mylist hello
(integer) 1

> BLPOP mylist 0
1) "mylist"
2) "hello"
```

The following content is written to the append-only file:

```
*3\r\n
$5\r\n
LPUSH\r\n
$6\r\n
mylist\r\n
$5\r\n
hello\r\n
*2\r\n
$4\r\n
LPOP\r\n
$6\r\n
mylist\r\n
```

The `BLPOP mylist 0` command is transformed to `LPOP mylist` before being written.

If `appendfsync` is `always`, each command is written to the append-only file before sending the response back to the client.

### Tests

The tester will create the following under `dir/appenddirname`:

- A manifest whose type `i` entry names a file `<random_file_name>.1.incr.aof`
- The append-only file mentioned in the manifest file: `<random_file_name>.1.incr.aof`.

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name> --appendfsync always
```

It will then send the following commands in order:

```bash
$ redis-cli LPUSH mylist element
$ redis-cli BLPOP mylist 0
```

The tester will expect both commands to be reflected in the append-only file:

```
*3\r\n
$5\r\n
LPUSH\r\n
$6\r\n
mylist\r\n
$7\r\n
element\r\n
*2\r\n
$4\r\n
LPOP\r\n
$6\r\n
mylist\r\n
```

Commands should not be directly byte-copied from the client's buffer to the append-only file.

Each command should be parsed, and at the time of writing to the append-only file, should be re-encoded as RESP and written to the file.

This is because some commands are transformed before being written to the append-only file, e.g. the `BLPOP key timeout` command is transformed to `LPOP key` before being written to the file.

### Notes

- You must read the manifest to determine which file to open for writes. The tester uses a non-default filename to ensure you follow the manifest and not only `<appendfilename>.1.incr.aof`.

- You don't need to implement the behavior of `--appendfsync everysec`.
 