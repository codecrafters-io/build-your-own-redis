In this stage, you'll implement the default values for three AOF-related options: `appendonly`, `appenddirname`, and `appendfilename`. 

### AOF Persistence

AOF (Append Only File) persistence records every write operation received by the server in a log file. When AOF is enabled, Redis appends each modifying command to the end of the file. On restart, the server replays the file to rebuild the in-memory state. This is useful for recovery if the server restarts or crashes.

- The `appendonly` option - Controls whether AOF persistence is enabled or disabled. Its default value is `no`.

- The `appenddirname` option - Specifies the directory name under `dir` where AOF files are stored. Its default value is `appendonlydir`.

- The `appendfilename` option - Sets the name of the AOF file. Its default value is `appendonly.aof`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send the following commands:

```bash
$ redis-cli CONFIG GET appendonly
$ redis-cli CONFIG GET appenddirname
$ redis-cli CONFIG GET appendfilename
```

Your server must respond to each `CONFIG GET` command with a RESP array containing two elements: the parameter name and its value, each encoded as a [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

For example, the expected responses for `CONFIG GET appendonly` is:

```
*2\r\n$10\r\nappendonly\r\n$2\r\nno\r\n
```

### Notes

- You don't need to set up anything related to AOF persistence at this stage, except for the default values of `appendonly`, `appenddirname`, and `appendfilename`. You will use these values when implementing AOF in later stages.