In this stage, you'll add type checking to the `HSET` command.

### The `WRONGTYPE` error

In Redis, every key has a single value type. Hash commands like `HSET` only operate on keys that hold a hash. If `HSET` is called against a key whose value is a string (or any other type), Redis replies with a specific RESP error:

```
WRONGTYPE Operation against a key holding the wrong kind of value
```

The error is sent as a [RESP simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors), and crucially the error prefix is `WRONGTYPE`, not `ERR`:

The key is left unchanged after the error — `HSET` must not overwrite the string with a hash, and must not modify it in any way.

```bash
> SET mykey somestring
OK
> HSET mykey field1 Hello
(error) WRONGTYPE Operation against a key holding the wrong kind of value
> GET mykey
"somestring"
```

`HSET` against a missing key still creates a fresh hash — the error only applies when the key already exists with a different type.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli SET mykey somestring
$ redis-cli HSET mykey field1 Hello
$ redis-cli GET mykey
```

The tester will verify that:

- `HSET mykey field1 Hello` returns a RESP simple error whose prefix is `WRONGTYPE` and whose message contains `key`, `value`, and `wrong`.

- After the error, `GET mykey` still returns the original bulk string `somestring` — the key is untouched.

### Notes

- The error prefix is `WRONGTYPE`, not `ERR`.

- `HSET` on a missing key (no existing value) must still succeed and create a hash, exactly like in the previous stage.
