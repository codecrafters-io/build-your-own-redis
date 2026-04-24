In this stage, you'll add support for the `HDEL` command.

### The `HDEL` Command

[`HDEL`](https://redis.io/docs/latest/commands/hdel/) removes one or more fields from a hash stored at a key. It returns the number of fields that were actually removed — fields that don't exist in the hash are silently ignored and don't count.

```bash
> HSET hash_key field1 "Hello" field2 "World"
(integer) 2

> HDEL hash_key field1
(integer) 1

> HDEL hash_key field1
(integer) 0          # field1 is already gone; nothing removed

> HDEL hash_key field2 field3
(integer) 1          # field2 removed, field3 didn't exist
```

If the key doesn't exist at all, `HDEL` returns `0`:

```bash
> HDEL missing_key field1
(integer) 0
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli HSET hash_key field1 Hello field2 World
$ redis-cli HDEL hash_key field1            # (integer) 1
$ redis-cli HDEL hash_key field1            # (integer) 0 — already removed
$ redis-cli HGET hash_key field1            # nil
$ redis-cli HDEL hash_key field2 missing_field     # (integer) 1 — missing_field doesn't exist
$ redis-cli HDEL missing_key field1             # (integer) 0 — key never existed
```

The tester will verify that:

- `HDEL` on an existing field returns `:1\r\n` and removes the field.
- `HDEL` on a field that doesn't exist returns `:0\r\n` without error.
- `HDEL` with multiple fields returns the count of fields actually removed (non-existent fields are not counted).
- `HDEL` on a non-existent key returns `:0\r\n`.
