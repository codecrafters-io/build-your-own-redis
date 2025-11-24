In this stage, you'll add support for reading the value corresponding to a key from an RDB file.

Just like with previous stages, we'll stick to supporting RDB files that contain a single key for now.

The tester will create an RDB file with a single key and execute your program like this:

```
./your_program.sh --dir <dir> --dbfilename <filename>
```

It'll then send a `GET <key>` command to your server.

```bash
$ redis-cli GET "foo"
```

The response to `GET <key>` should be a RESP bulk string with the value of the key.

For example, let's say the RDB file contains a key called `foo` with the value `bar`. The expected response will be `$3\r\nbar\r\n`.

Strings can be encoded in three different ways in the RDB file format:

- Length-prefixed strings
- Integers as strings
- Compressed strings

In this stage, you only need to support length-prefixed strings. We won't cover the other two types in this challenge.

We recommend using [this blog post](https://rdb.fnordig.de/file_format.html) as a reference when working on this stage.
