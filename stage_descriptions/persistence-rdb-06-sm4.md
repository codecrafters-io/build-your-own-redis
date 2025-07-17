In this stage, you'll add support for reading values that have an expiry set.

The tester will create an RDB file with multiple keys. Some of these keys will have an expiry set, and some won't. The expiry timestamps
will also be random, some will be in the past and some will be in the future.

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --dbfilename <filename>
```

It'll then send multiple `GET <key>` commands to your server.

```bash
$ redis-cli GET "foo"
$ redis-cli GET "bar"
```

When a key has expired, the expected response is `$-1\r\n` (a "null bulk string").

When a key hasn't expired, the expected response is a RESP bulk string with the value corresponding to the key.
