In this stage, you'll add support for the `EXISTS` command.

### The `EXISTS` Command

The [`EXISTS`](https://redis.io/docs/latest/commands/exists/) command checks whether a key exists in the database. It returns `1` if the key is present, `0` if it isn't.

```bash
> SET key "hello"
OK

> EXISTS key
(integer) 1

> EXISTS missing_key
(integer) 0
```

The `EXISTS` command works across all data types — it returns `1` if the key is present and `0` if it isn't, regardless of what type of value the key holds.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send commands such as:

```bash
$ redis-cli SET key "hello"           # OK
$ redis-cli EXISTS key                # (integer) 1
$ redis-cli GET key                   # "hello"
$ redis-cli EXISTS missing_key        # (integer) 0
```

The tester will verify that:

- `EXISTS key` returns `:1\r\n` for a key that was set with `SET`.
- `EXISTS missing_key` returns `:0\r\n` for a key that was never set.