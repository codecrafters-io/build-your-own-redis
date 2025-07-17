Welcome to the RDB Persistence Extension! In this extension, you'll add support for reading [RDB files](https://redis.io/docs/management/persistence/) (Redis Database files).

In this stage, you'll add support for two configuration parameters related to RDB persistence, as well as the [CONFIG GET](https://redis.io/docs/latest/commands/config-get/) command.

### RDB files

An RDB file is a point-in-time snapshot of a Redis dataset. When RDB persistence is enabled, the Redis server syncs its in-memory state with an RDB file, by doing the following:

1. On startup, the Redis server loads the data from the RDB file.
2. While running, the Redis server periodically takes new snapshots of the dataset, in order to update the RDB file.

### `dir` and `dbfilename`

The configuration parameters `dir` and `dbfilename` specify where an RDB file is stored:
- `dir` - the path to the directory where the RDB file is stored (example: `/tmp/redis-data`)
- `dbfilename` - the name of the RDB file (example: `rdbfile`)

### The `CONFIG GET` command

The [`CONFIG GET`](https://redis.io/docs/latest/commands/config-get/) command returns the values of configuration parameters.

It takes in one or more configuration parameters and returns a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays) of key-value pairs:

```bash
$ redis-cli CONFIG GET dir
1) "dir"
2) "/tmp/redis-data"
```

Although `CONFIG GET` can fetch multiple parameters at a time, the tester will only send `CONFIG GET` commands with one parameter at a time.

### Tests

The tester will execute your program like this:

```bash
./your_program.sh --dir /tmp/redis-files --dbfilename dump.rdb
```

It'll then send the following commands:

```bash
$ redis-cli CONFIG GET dir
$ redis-cli CONFIG GET dbfilename
```

Your server must respond to each `CONFIG GET` command with a RESP array containing two elements:

1. The parameter **name**, encoded as a [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings)
2. The parameter **value**, encoded as a RESP Bulk string

For example, if the value of `dir` is `/tmp/redis-files`, then the expected response to `CONFIG GET dir` is:

```bash
*2\r\n$3\r\ndir\r\n$16\r\n/tmp/redis-files\r\n
```

### Notes

- You don't need to read the RDB file in this stage, you only need to store `dir` and `dbfilename`. Reading from the file will be covered in later stages.
- If your repository was created before 5th Oct 2023, it's possible that your `./your_program.sh` script is not passing arguments to your program. To fix this, you'll need to edit `./your_program.sh`. Check the [update CLI args PR](https://github.com/codecrafters-io/build-your-own-redis/pull/89/files) for details on how to do this.
