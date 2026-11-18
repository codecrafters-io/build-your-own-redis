Your server creates a one-element list. In this stage, you'll let `RPUSH` append
another element while preserving the elements already stored under that key.

### Appending to an existing list

When a client sends [`RPUSH`](https://redis.io/docs/latest/commands/rpush/) on a
list that already exists, the new element is appended to the end of the list.

```bash
$ redis-cli RPUSH list_key "element1"
(integer) 1
$ redis-cli RPUSH list_key "element2"
(integer) 2
```

The server then returns the new length of the list as a
[RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send multiple `RPUSH` commands on the same list:

```bash
$ redis-cli RPUSH list_key "element1"
# Expect: (integer) 1 → encoded as :1\r\n

$ redis-cli RPUSH list_key "element2"
# Expect: (integer) 2 → encoded as :2\r\n
```

In each case, the tester will expect the response to be the length of the list
encoded as a RESP integer.
