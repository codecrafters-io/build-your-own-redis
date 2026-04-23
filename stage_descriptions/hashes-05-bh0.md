In this stage, you'll add support for the `HEXISTS` command.

### The `HEXISTS` Command

The [`HEXISTS`](https://redis.io/docs/latest/commands/hexists/) command checks whether a field is present in a hash. The reply is a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers):

- `1` if the field exists in the hash.
- `0` if the field is missing, or the key itself does not exist.

```bash
> HSET hash_key field1 "Hello"
(integer) 1
> HEXISTS hash_key field1
(integer) 1
> HEXISTS hash_key missing_field
(integer) 0
> HEXISTS nokey field1
(integer) 0
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli HSET hash_key field1 Hello
$ redis-cli HEXISTS hash_key field1
$ redis-cli HEXISTS hash_key missing_field
$ redis-cli HEXISTS nokey field1
```

The tester will verify that:

- `HEXISTS hash_key field1` returns `:1\r\n`.
- `HEXISTS hash_key missing_field` returns `:0\r\n`.
- `HEXISTS nokey field1` returns `:0\r\n`.
