This extension will let your server recover data by replaying saved write
commands. In this stage, you'll expose the default AOF configuration through
`CONFIG GET`, so clients can inspect how that persistence will be configured.

### AOF persistence in Redis

Redis serves data from memory. Without a saved copy, that data is lost when the
process stops. AOF (append-only file) persistence records commands that modify
data and replays the saved commands on startup to rebuild the database.

The AOF behavior is controlled by several configuration options. For this stage,
you'll set up their default values so they can be retrieved with `CONFIG GET`.
You don't need to implement any actual persistence logic yet.

Here are the options and their defaults:

| Option           | Purpose                                                                        | Default                              |
| ---------------- | ------------------------------------------------------------------------------ | ------------------------------------ |
| `dir`            | The base directory where Redis stores its data files                           | Current working directory at startup |
| `appendonly`     | Controls whether AOF persistence is enabled or disabled                        | `no`                                 |
| `appenddirname`  | The subdirectory under `dir` where AOF and manifest files are stored           | `appendonlydir`                      |
| `appendfilename` | The base name used for AOF and manifest filenames                              | `appendonly.aof`                     |
| `appendfsync`    | How often Redis asks the operating system to synchronize AOF writes to storage | `everysec`                           |

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then query each option using `CONFIG GET`:

```bash
$ redis-cli CONFIG GET dir
1) "dir"
2) "/path/to/current/directory"

$ redis-cli CONFIG GET appendonly
1) "appendonly"
2) "no"

$ redis-cli CONFIG GET appenddirname
1) "appenddirname"
2) "appendonlydir"

$ redis-cli CONFIG GET appendfilename
1) "appendfilename"
2) "appendonly.aof"

$ redis-cli CONFIG GET appendfsync
1) "appendfsync"
2) "everysec"
```

Each response is a RESP array with two
[bulk strings](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings):
the option name and its value. For example, `CONFIG GET appendonly` should
return:

```
*2\r\n$10\r\nappendonly\r\n$2\r\nno\r\n
```

### Notes

- If you've already implemented `CONFIG GET` in the RDB extension, it can also
  expose these options. Completing that extension isn't required for this task.
