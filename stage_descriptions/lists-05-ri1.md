In this stage, you will add support for negative indexes for the `LRANGE` command.

### `LRANGE` with negative indexes

The `LRANGE` command can also use negative indexes.

A negative index is an offset from the end of the list: `-1` refers to the last element, `-2` refers to the second-to-last, and so on.

For example:

```bash
# Create a list with 5 items
> RPUSH list_key "a" "b" "c" "d" "e"
(integer) 5

# List the last 2 items 
> LRANGE list_key -2 -1
1) "d"
2) "e"

# List all items except the last 2
> LRANGE list_key 0 -3
1) "a"
2) "b"
3) "c"
```

If a negative index is out of range (e.g., `-6` on a list of length `5`), it should be treated as `0` (the start of the list).

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then create a new list with multiple elements.

```bash
$ redis-cli RPUSH list_key "a" "b" "c" "d" "e"
```

The tester will then send your program a series of `LRANGE` commands with one or more negative indexes.

For example, the tester might send you this command:

```bash
$ redis-cli LRANGE list_key 2 -1
```

In this case, the tester expects the response to be the array `["c", "d", "e"]`, which is RESP encoded as:

```
*3\r\n
$1\r\n
c\r\n
$1\r\n
d\r\n
$1\r\n
e\r\n
``` 
