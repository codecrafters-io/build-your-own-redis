In this stage, you’ll add support for `RPUSH` to append a single element to an existing list.

### Appending to an Existing List

When a client sends [`RPUSH`](https://redis.io/docs/latest/commands/rpush/) on a list that already exists, the new element is appended to the end of the list. 
```bash
$ redis-cli RPUSH list_key "element1"
(integer) 1
$ redis-cli RPUSH list_key "element2"
(integer) 2
```
The server then returns the new length of the list as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

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

In each case, the tester will expect the response to be the length of the list encoded as a RESP integer. 

### Notes
- You'll need to check if a list already exists for the given key and append to it, rather than creating a new list.
