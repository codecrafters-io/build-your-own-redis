In this stage, you'll add support for reading a single field from a hash using the `HGET` command.

### The `HGET` Command

The [`HGET`](https://redis.io/docs/latest/commands/hget/) command returns the value associated with a field in a hash.

- If the field exists, the reply is a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings) with the value.
- If the field does not exist, or the key does not exist, the reply is a [RESP nil bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings) (`$-1\r\n`).

```bash
> HSET hash_key field1 "Hello"
(integer) 1

> HGET hash_key field1
"Hello"

> HGET hash_key missing_field
(nil)

> HGET missing_key field1
(nil)
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli HSET hash_key field1 Hello
$ redis-cli HGET hash_key field1
$ redis-cli HGET hash_key missing_field
$ redis-cli HGET missing_key field1
```

The tester will verify that:

- `HGET hash_key field1` returns the bulk string `Hello` (encoded as `$5\r\nHello\r\n`).
- `HGET hash_key missing_field` returns a RESP nil bulk string (`$-1\r\n`).
- `HGET missing_key field1` returns a RESP nil bulk string (`$-1\r\n`).