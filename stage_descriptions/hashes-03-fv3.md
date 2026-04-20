In this stage, you'll extend `HSET` to accept multiple field/value pairs in a single call.

### `HSET` with multiple pairs

`HSET` can take any number of `field value` pairs after the key:

```
HSET key field value [field value ...]
```

Each pair is applied in order. If a field already exists, its value is overwritten. The integer reply is the number of fields that were newly added, not the total number of pairs in the request and not the number of fields that were updated.

```bash
# All three fields are new
> HSET myhash field1 Hello field2 World field3 Foo
(integer) 3

# field2 is updated; field4 is new
> HSET myhash field2 Universe field4 Bar
(integer) 1
```

If the number of arguments after the key is not even (e.g. `HSET myhash a 1 b`), `HSET` must reply with a RESP error whose message indicates a wrong number of arguments for the `HSET` command.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli HSET myhash field1 Hello field2 World field3 bang
$ redis-cli HSET myhash field2 Universe field4 dot
$ redis-cli HSET myhash a 1 b
```

The tester will verify that:

- The first `HSET` returns `:3\r\n` — three new fields were added.
- The second `HSET` returns `:1\r\n` — only `field4` is new; updating `field2` does not count.
- The last `HSET` returns a RESP error reply. The error message must (case-insensitively) start with `wrong number of arguments` and contain `hset`. The exact wording is not asserted.

### Notes

- Updating an existing field is a no-op for the integer reply, but the value must still be overwritten.
