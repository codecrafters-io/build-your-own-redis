In this stage, you'll add support for the `HLEN` command.

### The `HLEN` Command

[`HLEN`](https://redis.io/docs/latest/commands/hlen/) returns the number of fields contained in the hash stored at the key. The reply is a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

If the key does not exist, the reply is `0`.

```bash
> HSET hash_key field1 "Hello" field2 "World"
(integer) 2
> HLEN hash_key
(integer) 2
> HLEN missing_key
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
$ redis-cli HLEN hash_key
$ redis-cli HLEN missing_key
```

The tester will verify that:

- `HLEN hash_key` returns `:2\r\n` — the exact number of fields in the hash.
- `HLEN missing_key` returns `:0\r\n`.
