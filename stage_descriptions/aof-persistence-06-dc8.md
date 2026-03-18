In this stage, you'll add support for restoring the database by replaying the append-only file.

### Replaying the append-only file

On startup with `--appendonly yes`, Redis reads the manifest file at `dir/appenddirname/appendfilename.manifest`. It parses each line to find the file entry whose type is `i` (incremental). That entry gives the name of the append-only file in the same directory. Redis opens that file and parses it as a sequence of RESP-encoded commands, replaying each command in order as if the client had sent it. After replaying all the commands, the database is restored.

### Tests

The tester will create a directory `<dir>/<appenddirname>`.

Inside the directory, it'll create an append-only file `<appendfilename>.1.incr.aof` that will contain one RESP-encoded command, `SET <key> <value>`

It will also create a metadata file `<appendfilename>.manifest`, which will contain the following line:

```
file <random_file_name>.1.incr.aof seq 1 type i
```

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

Your program should do the following:

- Read the manifest file `<dir>/<appenddirname>/<appendfilename>.manifest`
- Identify the `file` specified in the line inside the manifest, which is the append-only file.
- Read the append-only file, which will contain one command to be replayed.
- Replay the command as if it was sent by a client.

The tester will then create a new client and send a `GET` command:

```bash
$ redis-cli GET <key>
```

The value returned should be the value specified in the append-only file.

### Notes

- You should not directly read the file `<dir>/<appenddirname>/<appendfilename>.1.incr.aof`. You should read and replay commands from the file which is specified in the `<appendfilename>.manifest`.

- Redis always uses the format `file <appendfilename>.1.incr.aof seq 1 type i` in the manifest file. The tester will, however, use a different file name `file <random_file_name>.1.incr.aof seq 1 type i` to ensure that the file mentioned in the manifest is read, and not the one complying with the default format.