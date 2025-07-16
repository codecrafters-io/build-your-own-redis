In this stage, you'll add support for setting a key with an expiry.

The expiry for a key can be provided using the "PX" argument to the [SET](https://redis.io/commands/set) command. The expiry is provided in milliseconds.

```bash
$ redis-cli SET foo bar px 100 # Sets the key "foo" to "bar" with an expiry of 100 milliseconds
OK
```

After the key has expired, a `GET` command for that key should return a "null bulk string" (`$-1\r\n`).

{{#lang_is_haskell}}
The [time](https://hackage.haskell.org/package/time) package is available
to use as a dependency.
{{/lang_is_haskell}}

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send a `SET` command to your server to set a key with an expiry:

```bash
$ redis-cli SET foo bar px 100
```

It'll then immediately send a `GET` command to retrieve the value:

```bash
$ redis-cli GET foo
```

It'll expect the response to be `bar` (encoded as a RESP bulk string).

It'll then wait for the key to expire and send another `GET` command:

```bash
$ sleep 0.2 && redis-cli GET foo
```

It'll expect the response to be `$-1\r\n` (a "null bulk string").

### Notes

- Just like command names, command arguments are also case-insensitive. So `PX`, `px` and `pX` are all valid.
- The keys, values and expiry times used in the tests will be random, so you won't be able to hardcode a response to pass this stage.