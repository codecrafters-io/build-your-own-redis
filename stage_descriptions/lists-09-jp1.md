In this stage, you'll add support for removing multiple elements in a single `LPOP` command.

### `LPOP` with multiple elements

The `LPOP` command accepts an optional argument that specifies how many elements to remove from a list. The command returns the removed elements in a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays).

For example:

```bash
> RPUSH list_key "a" "b" "c" "d"
(integer) 4
> LPOP list_key 2
1) "a"
2) "b"
> LRANGE list_key 0 -1
1) "c"
2) "d"
```

In this case, `LPOP` removes the first two elements, `a` and `b`, and the list is left with `c` and `d`. 

If the number of elements to remove is greater than the list's length, the command should remove all elements in the list and return them as a RESP array.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will first create a list with multiple elements.

```bash
$ redis-cli
> RPUSH list_key "one" "two" "three" "four" "five"
# Expect: 5 (RESP Encoded Integer)
```

After that, it will send your program an `LPOP` command with the number of elements to remove.

```bash
> LPOP list_key 2
# Expect RESP Encoded Array: ["one", "two"]
```

The tester will verify that the response is a RESP array of the removed elements.

It will also use the `LRANGE` command to verify the remaining elements in the list.

```bash
> LRANGE list_key 0 -1
# Expect RESP Encoded Array: ["three", "four", "five"]
``` 
