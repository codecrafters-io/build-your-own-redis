In this stage, you'll add support for the [`HSETNX`](https://redis.io/docs/latest/commands/hsetnx/) command.

### The `HSETNX` Command

`HSETNX` sets a single field to a value, but only when the field does not already exist in the hash. If the key does not exist, a new hash is created.

```bash
> HSETNX myhash field1 Hello
(integer) 1

# field1 already exists; the call has no effect
> HSETNX myhash field1 World
(integer) 0

> HGET myhash field1
"Hello"
```

The integer reply is:

- `1` — the field was new and the value was stored.
- `0` — the field already existed and nothing was changed.

When the key does not exist, `HSETNX` must create the hash with the single field/value pair:

```bash
> HSETNX freshhash field1 v
(integer) 1
> HGET freshhash field1
"v"
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli HSETNX myhash field1 Hello
$ redis-cli HSETNX myhash field1 World
$ redis-cli HGET myhash field1
$ redis-cli HSETNX freshhash field1 v
```

The tester will verify that:

- The first `HSETNX` returns `:1\r\n` and the value is stored.
- The second `HSETNX` returns `:0\r\n`, and `HGET myhash field1` still returns the bulk string `Hello` (the value was not overwritten).
- `HSETNX freshhash field1 v` returns `:1\r\n` — the missing key is created as a new hash with the single field.
