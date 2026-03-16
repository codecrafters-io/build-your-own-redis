In this stage, you'll implement loading the initial dataset from the base RDB file inside the append-only directory.

### AOF directory structure

When AOF is enabled, Redis can use the `appenddirname` under `dir` to store multiple files:
- A manifest file used to store the layout of the directory
- A base RDB file which acts as a checkpoint for quickly loading the data
- An incremental `.incr` file, which stores raw commands after the checkpoint

On startup, if the server was started with `--appendonly yes`, it must look for an AOF directory at `dir`/`appenddirname`. If that directory exists, the server should read the manifest file to discover which file contains the base snapshot to load. The manifest file is named after the append filename: `<appendfilename>.manifest` (for example, if `appendfilename` is `appendonly.aof`, the manifest is `appendonly.aof.manifest`). If there is no AOF directory or no manifest, the server starts with an empty dataset.

### Manifest file format

The manifest is a text file, one entry per line. Each line that describes a file has the form:

```
file <filename> seq <number> type <b|h|i>
```

- `file` is the literal word "file", followed by the filename (relative to the AOF directory), then `seq`, a sequence number, then `type`.

- The `seq` will always be 1.

- `type b` means the current base RDB file: the snapshot you must read to load the initial keys and values. This is the only entry you use for loading in this stage.

- `type i` means an incremental AOF file. We'll get to this in the later stages.


Example manifest content:

```
file appendonly.aof.1.incr.aof seq 1 type i
file appendonly.aof.1.base.rdb seq 1 type b
```

Here, the line ending in `type b` refers to the base RDB file `app.aof.1.base.rdb`. Your server should read `dir`/`appenddirname`/`app.aof.1.base.rdb` and load the keys and values from it using the same RDB parsing logic as in the RDB persistence stages.

### Tests

The tester will create the AOF directory structure (including the manifest and a base RDB file), then run your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <appenddirname> --appendfilename <appendfilename>
```

It will then send `GET` commands to check that the keys loaded from the base RDB have the expected values. Your server must respond to each `GET` with the value stored for that key (as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings)), or with a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`) if the key does not exist.

### Notes

- You only need to support loading from the base RDB file (type `b`) in this stage. Ignore the type `i` (incremental); incremental AOF will be handled in the later stages.

- RDB parsing is the same as in the RDB persistence extension; reuse your existing RDB file reader to interpret the base file.
