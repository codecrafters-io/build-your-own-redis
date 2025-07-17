In this stage, you'll add support for the [SET](https://redis.io/commands/set) &
[GET](https://redis.io/commands/get) commands.

The `SET` command is used to set a key to a value. The `GET` command is used to retrieve the value of a key.

```bash
$ redis-cli SET foo bar
OK
$ redis-cli GET foo
bar
```

The `SET` command supports a number of extra options like `EX` (expiry time in seconds), `PX` (expiry time in milliseconds) and more. We
won't cover these extra options in this stage. We'll get to them in later stages.

### Tests

The tester will execute your program like this:

```bash
./your_program.sh
```

It'll then send a `SET` command to your server:

```bash
$ redis-cli SET foo bar
```

The tester will expect to receive `+OK\r\n` as a response (that's the string `OK` encoded as a [RESP simple string](https://redis.io/docs/reference/protocol-spec/#resp-simple-strings)).

This command will be followed by a `GET` command:

```bash
$ redis-cli GET foo
```

The tester will expect to receive `$3\r\nbar\r\n` as a response (that's the string `bar` encoded as a [RESP bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings).

### Notes

- If you implemented a proper Redis protocol parser in the previous stage, you should be able to reuse it in this stage.
- Just like the previous stage, the values used for keys and values will be random, so you won't be able to hardcode the response to pass this stage.
- If a key doesn't exist, the `GET` command should return a "null bulk string" (`$-1\r\n`). We won't explicitly test this in this stage, but you'll need it for the next stage (expiry).