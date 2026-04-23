In this stage, you'll handle automatic key deletion when all fields are removed from a hash.

### Auto-Deletion of Empty Hashes

Redis automatically deletes a hash key when the last field is removed. This happens as a side effect of `HDEL` — when the field count drops to zero, the key itself disappears from the keyspace.

```bash
> HSET hash_key only "one"
(integer) 1

> HDEL hash_key only
(integer) 1

> EXISTS hash_key
(integer) 0          # key is gone
```

This also applies when you delete multiple fields in one call and they happen to be the last ones:

```bash
> HSET hash_key a "1" b "2"
(integer) 2

> HDEL hash_key a b
(integer) 2

> EXISTS hash_key
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
$ redis-cli HDEL hash_key field1      # (integer) 1 — last field removed
$ redis-cli EXISTS hash_key           # (integer) 0 — key auto-deleted
$ redis-cli HSET hash_key a 1 b 2
$ redis-cli HDEL hash_key a b         # (integer) 2 — both fields removed
$ redis-cli EXISTS hash_key           # (integer) 0 — key auto-deleted again
```

The tester will verify that:

- After `HDEL` removes the last field, `EXISTS hash_key` returns `0`.
- When multiple fields are deleted in one `HDEL` call and all remaining fields are removed, the key is also deleted.
