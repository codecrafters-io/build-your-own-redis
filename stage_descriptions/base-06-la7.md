`ECHO` returns data from the current request. In this stage, you'll implement
[SET](https://redis.io/commands/set) and [GET](https://redis.io/commands/get) so
your server can keep a value and return it in a later request.

### The `SET` command

The `SET` command is used to set a key to a value. For example, a client can set
a key `foo` to a value `bar` like this:

```bash
$ redis-cli SET foo bar
OK
```

The server then responds with `OK` (as a
[RESP simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings))
to confirm that the action was successful.

`SET` stores the supplied value under the key, replacing any previous value.
Redis also supports options such as `EX` (expiry in seconds) and `PX` (expiry in
milliseconds). These options are outside this stage's scope.

### The `GET` command

The `GET` command is used to retrieve the value of a key. For example, to
retrieve the value for the key `foo`:

```bash
$ redis-cli GET foo
bar
```

The server responds with the value `bar` encoded as a
[RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).
If the key doesn't exist, the server responds with a special
[null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings)
(`$-1\r\n`).

### Tests

The tester will execute your program like this:

```bash
./your_program.sh
```

It'll then send a `SET` command to your server:

```bash
$ redis-cli SET foo bar
```

The tester will expect to receive `+OK\r\n` as a response. That's the string
`OK` encoded as a
[simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings).

This command will be followed by a `GET` command:

```bash
$ redis-cli GET foo
```

The tester will expect to receive `$3\r\nbar\r\n` as a response. That's the
string `bar` encoded as a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- These commands use the same RESP array format as `ECHO`.
- Just like previous stages, the keys and values will be random, so you won't be
  able to hardcode the response to pass this stage.
- The missing-key behavior described above is required even though this stage's
  tests focus on retrieving a stored value. Expiry will rely on it later.
