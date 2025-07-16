In this stage, you'll add support for the `LPUSH` command, which prepends elements to a list.

### The `LPUSH` Command

The `LPUSH` command is similar to `RPUSH`, except that it inserts elements from the left rather than right. If a list doesn't exist, it is created first before prepending elements.

Example usage:

```bash
> LPUSH list_key "a" "b" "c"
(integer) 3

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

It will then send a series of `LPUSH` commands and expect the response to be the list length in each case, which is a RESP integer.

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