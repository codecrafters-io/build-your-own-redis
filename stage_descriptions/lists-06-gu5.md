In this stage, you'll add support for the `LPUSH` command, which prepends elements to a list.

### The `LPUSH` Command

The `LPUSH` command is similar to `RPUSH`, except that it inserts elements at the head of the list instead of the tail. If the list doesn't exist, it gets created first before prepending the elements.

For example, when you use `LPUSH`:

```bash
> LPUSH list_key "a" "b" "c"
(integer) 3
```

Even though the elements were listed as "a", "b", "c", the `LRANGE` command shows they were added to the list in reverse order, so the list becomes `["c", "b", "a"]`.

```bash
> LRANGE list_key 0 -1
1) "c"
2) "b"
3) "a"
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send a series of `LPUSH` commands. For each command, the tester will expect the response to be the list length as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

```bash
$ redis-cli
> LPUSH list_key "c"
# Expect: (integer) 1

> LPUSH list_key "b" "a"
# Expect: (integer) 3
```

It'll also use the `LRANGE` command to verify that elements are inserted in the correct order.

```bash
> LRANGE list_key 0 -1
# Expect RESP Encoded Array: ["a", "b", "c"]
``` 

### Notes
- Elements are inserted at index `0`, shifting all existing elements to higher indexes.
