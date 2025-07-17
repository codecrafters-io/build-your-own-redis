In this stage, you'll add support for reading multiple keys from an RDB file.

The tester will create an RDB file with multiple keys and execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --dbfilename <filename>
```

It'll then send a `KEYS *` command to your server.

```bash
$ redis-cli KEYS "*"
```

The response to `KEYS *` should be a RESP array with the keys as elements.

For example, let's say the RDB file contains two keys: `foo` and `bar`. The expected response will be:

```
*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n
```

- `*2\r\n` indicates that the array has two elements
- `$3\r\nfoo\r\n` indicates that the first element is a bulk string with the value `foo`
- `$3\r\nbar\r\n` indicates that the second element is a bulk string with the value `bar`
