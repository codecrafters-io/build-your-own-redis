In this stage, you'll add support for fetching multiple fields in one call using the `HMGET` command.

### The `HMGET` Command

The [`HMGET`](https://redis.io/docs/latest/commands/hmget/) command returns the values of the specified fields, in the same order they were requested. Missing fields are returned as `nil`. If the key does not exist, every position in the reply is `nil`.

```bash
> HSET myhash field1 "Hello" field2 "World"
(integer) 2
> HMGET myhash field1 missing_field field2
1) "Hello"
2) (nil)
3) "World"

> HMGET missing_key f1 f2
1) (nil)
2) (nil)
```

The reply is a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays). Each element is either a bulk string (for an existing field) or a nil bulk string (`$-1\r\n`) for a missing field.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli HSET myhash field1 Hello field2 World
$ redis-cli HMGET myhash field1 missing_field field2
$ redis-cli HMGET missing_key f1 f2
```

The tester will verify that:

- The reply to `HMGET myhash field1 missing_field field2` is a RESP array of length `3`, in this exact order: `Hello`, nil bulk string, `World`.
- The reply to `HMGET missing_key f1 f2` is a RESP array of length `2`, with both elements as nil bulk strings (`$-1\r\n`).
- Missing fields are encoded as nil bulk strings, not as empty bulk strings (`$0\r\n\r\n`) and not omitted from the array.

### Notes

- The order of the returned values must match the order of the requested fields, even when some are missing.
