In this stage, you'll add support for setting a key with an expiry.

### `SET` Command Options

The `SET` command can accept [optional arguments](https://redis.io/docs/latest/commands/set/#options) to modify its behaviour.

For example, here are a few options you can use with `SET`:

```bash
# Only set the key if it doesn't already exist.
$ redis-cli SET mykey value NX 
OK

# Only set the key if it already exists.
$ redis-cli SET mykey new_value XX 
OK
```

### The `PX` Option

The `PX` option is used to set a key's expiry time in milliseconds. After the key expires, it's no longer accessible.

For example, a client can set a key with an expiry like this:
```bash
$ redis-cli SET foo bar PX 100
OK
```
This command sets the key `foo` to the value `bar` with an expiry of 100 milliseconds.

After the key expires, a `GET` command for that key should return a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`).

```bash
$ redis-cli GET foo
(nil)
```

{{#lang_is_haskell}}
The [time](https://hackage.haskell.org/package/time) package is available
to use as a dependency.
{{/lang_is_haskell}}

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

Next, it will send a `SET` command to your server to set a key with an expiry:

```bash
$ redis-cli SET foo bar PX 100
```

Immediately after that, it will send a `GET` command to retrieve the value of the key:

```bash
$ redis-cli GET foo
```

The tester will expect to receive `$3\r\nbar\r\n` as a response. That's the string `bar` encoded as a bulk string.

After waiting for the key to expire, it will send another `GET` command:

```bash
$ sleep 0.2 && redis-cli GET foo
```

The tester will expect the response to be a null bulk string (`$-1\r\n`).

### Notes

- Just like command names, command arguments are also case-insensitive. So `PX`, `px`, and `pX` are all valid.
- The keys, values, and expiry times used in the tests will be random, so you won't be able to hardcode a response to pass this stage.
