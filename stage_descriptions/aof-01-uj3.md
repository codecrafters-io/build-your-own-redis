In this stage, you'll set up default values for AOF-related configuration options.

### AOF Persistence in Redis

Redis keeps all its data in memory, which makes it fast but vulnerable. If the server crashes, everything is gone. AOF (Append Only File) is one way to solve this. When enabled, the server logs every write command to a file, and on restart, it replays the file to rebuild its state.

The AOF behavior is controlled by several configuration options. For this stage, you'll set up their default values so they can be retrieved with `CONFIG GET`. You don't need to implement any actual persistence logic yet.

Here are the options and their defaults:

| Option | Purpose | Default |
|---|---|---|
| `dir` | The base directory where Redis stores its data files | Current working directory at startup |
| `appendonly` | Controls whether AOF persistence is enabled or disabled | `no` |
| `appenddirname` | The subdirectory under `dir` where AOF and manifest files are stored | `appendonlydir` |
| `appendfilename` | The name of the append-only file that records write operations | `appendonly.aof` |
| `appendfsync` | How often buffered writes are flushed to the AOF file on disk | `everysec` |

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

Each response is a RESP array with two [bulk strings](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings): the option name and its value. For example, `CONFIG GET appendonly` should return:
 
```
*2\r\n$10\r\nappendonly\r\n$2\r\nno\r\n
```

The tester will verify that:

- Each `CONFIG GET` returns a two-element RESP array
- The option name and default value are correct for each option

### Notes

- You don't need to implement any AOF persistence logic in this stage. Just store and return the default values.
- If you've already implemented `CONFIG GET` for other options in a previous challenge, you may just need to add these new keys to your existing configuration.
