In this stage, you'll add support for removing multiple elements in a single `LPOP` command.

### LPOP with multiple elements

The LPOP command accepts an optional argument to specify how many elements are to be removed.

Example usage:

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

If the number of elements to remove is greater than the list length, it returns RESP encoded array of all the elements of the list.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will create a list with multiple elements in it.

```bash
$ redis-cli
> RPUSH list_key "one" "two" "three" "four" "five"
# Expect: 4 (Resp Encoded Integer)
```

After that it will send your program a `LPOP` command with the number of elements to remove.

```bash
> LPOP list_key 2
# Expect RESP Encoded Array: ["one", "two"]
```

The tester will verify that the response is a RESP encoded array of removed elements.

It will also use the `LRANGE` command to verify the remaining elements in the list.

```bash
> LRANGE list_key 0 -1
# Expect RESP Encoded Array: ["three", "four", "five"]
``` 