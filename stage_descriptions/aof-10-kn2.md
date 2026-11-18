Your server restores one saved command. In this stage, you'll replay every
command in the AOF in order, so a later write can update a value restored by an
earlier one.

### Replaying multiple commands

An AOF is a sequence of RESP-encoded commands. Applying them in file order must
produce the same database state as processing that sequence from a client.

The `*<n>\r\n` prefix gives the number of elements in a command's array. Each
bulk string then supplies its own byte length. Together, these fields mark
command boundaries without extra separators. Continue through the end of the
file.

For example, if the file contains `SET foo 100` followed by `SET foo 200`, the
restored value of `foo` is `200`.

### Tests

The tester will create a directory `<dir>/<append_dir_name>` containing:

- An AOF file `<random_file_name>.1.incr.aof` with multiple RESP-encoded
  commands (e.g., several `SET` commands for different keys)
- A manifest file `<append_file_name>.manifest` pointing to that AOF file:

```
file <random_file_name>.1.incr.aof seq 1 type i
```

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

After startup, the tester will send several `GET` commands to verify each key
was restored:

```bash
$ redis-cli GET <key1>
$ redis-cli GET <key2>
...
```

The tester will verify that:

- Each `GET` returns the value from the corresponding command in the AOF file
- Later writes to the same key replace the values restored by earlier writes

### Notes

- If your implementation from earlier stages already loops through the file
  until EOF, this stage may already pass without changes.
- The manifest's active filename can differ from
  `<append_file_name>.1.incr.aof`. Continue using the name from the manifest.
