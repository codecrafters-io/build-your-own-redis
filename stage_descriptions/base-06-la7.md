In this stage, you'll add support for the [SET](https://redis.io/commands/set) &
[GET](https://redis.io/commands/get) commands.

### The `SET` Command

The `SET` command is used to set a key to a value. For example, a client can set a key `foo` to a value `bar` like this:
```bash
$ redis-cli SET foo bar
OK
```
The server then responds with `OK` (as a [RESP simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings)) to confirm that the action was successful.

The `SET` command supports a number of extra options like `EX` (expiry time in seconds), `PX` (expiry time in milliseconds), and more. We
won't cover these extra options in this stage. We'll get to them in later stages.

### The `GET` Command

The `GET` command is used to retrieve the value of a key. For example, to retrieve the value for the key `foo`:
```bash
$ redis-cli GET foo
bar
```
The server responds with the value `bar` encoded as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings). If the key doesn't exist, the server responds with a special [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`).

### Tests

The tester will execute your program like this:

```bash
./your_program.sh
```

It'll then send a `SET` command to your server:

```bash
$ redis-cli SET foo bar
```

The tester will expect to receive `+OK\r\n` as a response. That's the string `OK` encoded as a [simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings).

This command will be followed by a `GET` command:

```bash
$ redis-cli GET foo
```

The tester will expect to receive `$3\r\nbar\r\n` as a response. That's the string `bar` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- If you implemented a proper RESP parser in the previous stage, you should be able to reuse it in this stage.
- Just like the previous stage, the values used for keys and values will be random, so you won't be able to hardcode the response to pass this stage.
- If a key doesn't exist, the `GET` command should return a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`). We won't explicitly test this in this stage, but you'll need it for the next stage.
