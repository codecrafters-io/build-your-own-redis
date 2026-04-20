In this stage, you'll implement the `HSET` command.

### `HSET` with one pair

`HSET` returns the number of fields newly added to the hash. With a single field/value pair:

- If the field did not exist before, the reply is `1`.
- If the field already existed and is being overwritten, the reply is `0`.

```bash
> HSET myhash field1 Hello
(integer) 1

# Same field again — it's being updated, not added
> HSET myhash field1 World
(integer) 0

# Different field on the same hash — newly added
> HSET myhash field2 Hello
(integer) 1
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli HSET myhash field1 Hello
$ redis-cli HSET myhash field1 World
$ redis-cli HSET myhash field2 bang
```

The tester will verify that:

- The first `HSET` returns `:1\r\n` (the field is new).
- The second `HSET` on the same field returns `:0\r\n` (the field already exists; it is updated, not added).
- The third `HSET` on a different field on the same key returns `:1\r\n`.

### Notes

- Each `HSET` in this stage still uses exactly one field/value pair. Multiple pairs in a single call are covered in a later stage.
