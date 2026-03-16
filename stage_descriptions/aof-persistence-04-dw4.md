In this stage, you'll implement loading the dataset by replaying commands from an incremental AOF file. The tester creates a manifest with one type `i` entry and an incremental AOF file that contains commands for a single key; you must replay the file and respond correctly when the tester checks that key with `GET`.

### AOF directory structure (incremental only)

When AOF is enabled, the server looks for the AOF directory at `dir`/`appenddirname` and reads the manifest file `<appendfilename>.manifest`. In this stage, the manifest will contain exactly one file entry and it will have type `i` (incremental). There will be no type `b` (base RDB) entry, so you do not load from any RDB file. You start with an empty dataset and rebuild it by replaying the commands stored in the incremental AOF file.

### Replaying the incremental AOF file

The incremental AOF file is an append-only log of write commands. Each command is stored in [RESP](https://redis.io/docs/latest/develop/reference/protocol-spec/) format (typically as a RESP array). Read the file from start to end and parse it as a sequence of RESP messages; for each command, apply it to the in-memory dataset as if the client had sent it. Replay commands in the order they appear in the file. The path to the incremental file is `dir`/`appenddirname`/`<filename>`, where `<filename>` is the one listed in the manifest entry with type `i`.

### Manifest format (one type `i` entry)

The manifest is a text file, one entry per line. Each line has the form:

```
file <filename> seq <number> type i
```

In this stage the tester only creates a manifest with a single entry of type `i`. Parse that line to get the filename and read that file from the AOF directory to replay the commands.

Example manifest content for this stage:

```
file appendonly.aof.1.incr.aof seq 1 type i startoffset 0 endoffset 1234
```

Your server should read `dir`/`appenddirname`/`appendonly.aof.1.incr.aof` and replay the RESP-encoded commands in order.

### Tests

The tester will create the AOF directory structure (manifest with one type `i` entry and the corresponding incremental AOF file; no RDB file). The incremental file will contain commands that result in a single key being set. The tester will then run your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <appenddirname> --appendfilename <appendfilename>
```

It will then send a `GET` command to check that the single key has the expected value after replay. Your server must respond with the value stored for that key (as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings)), or with a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`) if the key does not exist.

### Notes

- You do not need to support a base RDB (type `b`) in this stage; the manifest will only contain one type `i` entry.
- Replay only the commands needed to pass the tests (e.g. at least `SET` and any others used by the tester). You can extend to other write commands as needed.
- If `--appendonly` is not `yes`, or the AOF directory or manifest is missing, start with an empty dataset.
