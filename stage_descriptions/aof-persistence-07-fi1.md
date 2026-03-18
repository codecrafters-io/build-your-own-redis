In this stage, you'll extend replay of the append-only file to support multiple commands.

### Replaying the append-only file

On startup with `--appendonly yes`, Redis reads the manifest file, finds the append-only file listed with type `i`, and replays its commands one by one as if they were sent by a client.

### Tests

The tester will create a directory `dir`/`appenddirname`.

Inside the directory, it will create an append-only file (the name is given in the manifest) that contains multiple RESP-encoded commands (e.g. several `SET` commands for different keys).

It will also create a metadata file `appendfilename.manifest`, which will contain a line such as:

```
file <filename>.1.incr.aof seq 1 type i
```

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

Your program should do the following:

- Read the manifest file at `dir`/`appenddirname`/`appendfilename.manifest`.
- Find the file entry with type `i` and open that append-only file from the AOF directory.
- Read the append-only file and parse it as a sequence of RESP-encoded commands.
- Replay each command in order as if it was sent by a client.

The tester will then create a client and send several `GET` commands. It'll expect the value of the keys to be set as if the commands in the `.aof` file were sent by a client earlier.

### Notes

- You should read and replay from the file whose name is specified in the manifest, not from the hardcoded filename `dir/appenddirname/appendfilename.1.incr.aof`.
