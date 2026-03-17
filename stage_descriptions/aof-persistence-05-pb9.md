In this stage, you'll add support for creating the metadata file when append-only mode is enabled.

### The append-only file

If `--appendonly yes` flag has been specified, and the `appenddirname` does not exist inside the `dir` directory, Redis creates the directory. Inside the directory, the append-only file with the name `<appendfilename>.1.incr.aof` is also created. Along with the append-only file, Redis also creates a metadata file, which stores the details about the append-only file.

The metadata file is named `<appendfilename>.manifest`.

The metadata file contains one line, which is:

```
file <appendfilename>.1.incr.aof seq 1 type i
```

- `file <appendfilename>.1.incr.aof` — Literal `file` followed by the base name of the append-only file.
- `seq 1` — Sequence number for this file; You can hardcode this to `1` for now
- `type i` - Specifying that the file is an `incremental` file, or append-only file, from which the command is to be replayed.

You can hardcode the sequence and format for this stage.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

It will then check the following

- The directory `<dir>/<append_dir_name>` is created
- The empty file `<dir>/<append_dir_name>/<append_file_name>` is created
- The manifest file `<dir>/<append_dir_name>/<appendfilename>.manifest` is created
- The manifest file contains the line `file <appendfilename>.1.incr.aof seq 1 type i`
