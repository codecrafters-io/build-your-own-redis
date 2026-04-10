In this stage, you'll extend replay to handle append-only files with multiple commands.

### Replaying Multiple Commands

An append-only file typically contains many commands, not just one. Your server needs to parse the file as a sequence of RESP-encoded commands and replay each one in order, just as if a client had sent them live.

Since RESP is self-framing (each command starts with `*<n>\r\n` indicating its length), you can read commands one after another until you reach the end of the file. Each command you parse should be executed against your server's state before moving on to the next.

After replaying all commands, your database should reflect the cumulative effect of every write in the file.

### Tests

The tester will create a directory `<dir>/<append_dir_name>` containing:

- An AOF file `<random_file_name>.1.incr.aof` with multiple RESP-encoded commands (e.g., several `SET` commands for different keys)
- A manifest file `<append_file_name>.manifest` pointing to that AOF file:

```
file <random_file_name>.1.incr.aof seq 1 type i
```

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

After startup, the tester will send several `GET` commands to verify each key was restored:

```bash
$ redis-cli GET <key1>
$ redis-cli GET <key2>
...
```

The tester will verify that:

- Each `GET` returns the value from the corresponding command in the AOF file
- All commands from the AOF file were replayed, not just the first one

### Notes

- If your implementation from earlier stages already loops through the file until EOF, this stage may already pass without changes.
- You must read and replay from the file named in the manifest, not from the hardcoded filename `<append_file_name>.1.incr.aof`.
- RESP is self-framing, so you don't need any separator between commands. Just keep parsing until you run out of bytes.
