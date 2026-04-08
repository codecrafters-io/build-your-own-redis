In this stage, you'll create a manifest file alongside the append-only file.

### The Manifest File

In earlier stages, you created the append-only directory and an empty AOF file inside it. Now you'll also create a manifest file that tells Redis which AOF files exist and in what order they should be replayed.

The manifest file is named `<appendfilename>.manifest` and lives in the same directory as the AOF file. It contains a single line describing the AOF file you created earlier:

```
file <appendfilename>.1.incr.aof seq 1 type i
```

Here's what each part means:

| Field | Value | Meaning |
|---|---|---|
| `file` | `<appendfilename>.1.incr.aof` | The name of the AOF file |
| `seq` | `1` | Sequence number for this file |
| `type` | `i` | An incremental file, meaning it contains write commands appended one by one |

For example, if the server is started with `--appendfilename appendonly.aof`, the manifest file would be named `appendonly.aof.manifest` and should contain:

```
file appendonly.aof.1.incr.aof seq 1 type i
```

You can hardcode the sequence number and type for this stage.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --appendonly yes --appenddirname <append_dir_name> --appendfilename <append_file_name>
```

The tester will verify that:

- The directory `<dir>/<append_dir_name>` exists
- The empty AOF file `<dir>/<append_dir_name>/<append_file_name>.1.incr.aof` exists
- The manifest file `<dir>/<append_dir_name>/<append_file_name>.manifest` exists
- The manifest file contains the line `file <append_file_name>.1.incr.aof seq 1 type i`

### Notes

- The manifest file should be created at startup alongside the AOF file.
- Each token in the manifest line is separated by a single space. No tabs or extra spaces.
- Make sure the manifest line ends with a newline character.
