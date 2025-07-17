In this stage, you'll add support for the `TYPE` command.

### The TYPE command

The [TYPE](https://redis.io/commands/type/) command returns the type of value stored at a given key.

It returns one of the following types: string, list, set, zset, hash, and stream.

Here's how it works:

```bash
$ redis-cli SET some_key foo
"OK"
$ redis-cli TYPE some_key
"string"
```

If a key doesn't exist, the return value will be "none".

```bash
$ redis-cli TYPE missing_key
"none"
```

The return value is encoded as a [simple string](https://redis.io/docs/reference/protocol-spec/#simple-strings).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send a `SET` command to your server.

```bash
$ redis-cli SET some_key foo
```

It'll then send a `TYPE` command to your server.

```bash
$ redis-cli TYPE some_key
```

Your server should respond with `+string\r\n`, which is `string` encoded as a [RESP simple string](https://redis.io/docs/reference/protocol-spec/#simple-strings).

It'll then send another `TYPE` command with a missing key.

```bash
$ redis-cli TYPE missing_key
```

Your server should respond with `+none\r\n`, which is `none` encoded as a [RESP simple string](https://redis.io/docs/reference/protocol-spec/#simple-strings).

### Notes

- For now, you only need to handle the "string" and "none" types. We'll add support for the "stream" type in the next stage.
