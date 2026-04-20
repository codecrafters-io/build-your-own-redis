In this stage, you'll add support for the [`HSET`](https://redis.io/docs/latest/commands/hset/) command.

### The `HSET` Command

`HSET` sets the value of a field in a hash stored at a key. Its syntax is:

```
HSET key field value
```

The reply is a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers): the number of fields that were newly added by the call.

In this stage, you only need to acknowledge the command. The tester will only call `HSET` with a single field/value pair on a fresh key, so the correct reply is always `1`.

```bash
> HSET myhash field1 "Hello"
(integer) 1
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send the following command:

```bash
$ redis-cli HSET myhash field1 value1
```

The tester will verify that the response is `:1\r\n` — the integer `1` encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

### Notes

- You don't need to store the field/value yet. We'll get to the implementation in the later stages.

- You can hardcode the response to `:1\r\n` in this stage.