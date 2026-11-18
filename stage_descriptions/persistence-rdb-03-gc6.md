Your server can list a key from the snapshot. In this stage, you'll restore its
string value from a single-key snapshot so `GET` can return data saved before
startup.

### Tests

The tester will create an RDB file with a single key and execute your program
like this:

```
./your_program.sh --dir <dir> --dbfilename <filename>
```

It'll then send a `GET <key>` command to your server.

```bash
$ redis-cli GET "foo"
```

The response to `GET <key>` should be a RESP bulk string with the value of the
key.

For example, let's say the RDB file contains a key called `foo` with the value
`bar`. The expected response will be `$3\r\nbar\r\n`.

### Notes

Strings can be encoded in three different ways in the RDB file format:

- Length-prefixed strings
- Integers as strings
- Compressed strings

The key's value in this stage is a length-prefixed string. Integer-encoded
metadata may still appear elsewhere in the file, as shown in the earlier
reference. Compressed strings are outside this extension's scope.

Use the [RDB file format reference](https://rdb.fnordig.de/file_format.html)
when decoding the file.
