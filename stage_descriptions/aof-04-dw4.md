In this stage, you'll add support for creating the append-only file when append-only mode is enabled.

### The append-only file

If the `--appendonly yes` flag has been specified, and the directory `appenddirname` does not exist inside the `dir` directory, Redis creates the directory. Inside that directory, Redis creates an empty file named `<appendfilename>.1.incr.aof`.

This file is the append-only file which Redis uses to store the command logs, which can be used later for restoring the key-value pairs.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

It will then check the following:

- The directory `<dir>/<append_dir_name>` is created
- An empty file `<dir>/<append_dir_name>/<append_file_name>.1.incr.aof` is created

### Notes

- You don't need to create any files other than the append-only file in this stage. We'll get to creating other files in the later stages.