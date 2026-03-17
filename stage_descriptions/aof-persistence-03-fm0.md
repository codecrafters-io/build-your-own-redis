In this stage, you'll add support for creating append-only directory when append-only mode is enabled.

### The append-only directory

If `--appendonly yes` flag has been specified, and the `appenddirname` does not exist inside the `dir` directory, Redis creates the directory.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

It will then check whether the following directory has been created:

- `<dir>/<append_dir_name>`

### Notes

- You don't have to create the contents inside the directory in this stage. We'll get to that in the later stages.