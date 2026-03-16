In this stage, you'll combine loading from a base RDB file and replaying an incremental AOF file so that the server correctly restores the dataset when the manifest contains both type `b` and type `i` entries.

### Loading from base RDB and incremental AOF

When the manifest has both a type `b` (base RDB) entry and a type `i` (incremental AOF) entry, load the dataset in two steps. First, load the snapshot from the base RDB file (the file named in the type `b` entry) using the same RDB parsing as in stage 3. Then, replay the incremental AOF file in order: parse it as a sequence of RESP-encoded commands and apply them to the in-memory dataset, as in stages 4 and 5.

The final key-value pairs obtained using `GET` must reflect the combined effect of the base snapshot and the replayed incremental commands.

### Tests

The tester will create the AOF directory structure (manifest, base RDB file, and incremental AOF file), then run your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <appenddirname> --appendfilename <appendfilename>
```

It will then send `GET` commands to check that the keys have the expected values after loading the base RDB and replaying the incremental file. Your server must respond to each `GET` with the value stored for that key (as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings)), or with a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`) if the key does not exist.

### Notes

- Reuse the manifest parsing and RDB loading from stage 3 and the incremental replay logic from stages 4 and 5.
- If `--appendonly` is not `yes`, or the AOF directory or manifest is missing, start with an empty dataset.
