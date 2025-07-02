In this stage, you'll implement support for removing a single element from the left using the `LPOP` command.

### The `LPOP` Command

The `LPOP` command removes and returns the first element of the list. If the list is empty or doesn't exist, it returns a null bulk string (`$-1\r\n`).

Example usage:

```bash
> RPUSH list_key "a" "b" "c" "d"
(integer) 4

> LPOP list_key
"a"
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then create a list with some elements.

```bash
$ redis-cli
> RPUSH list_key "one" "two" "three" "four" "five"
```

It will then send an `LPOP` command to your server specifying the list that was just created.

```bash
> LPOP list_key
# Expecting: (Bulk string) "one"
```

The tester will expect the removed element to be returned, which is encoded as a RESP Bulk string (`$3\r\none\r\n`).

The tester will also verify that the remaining elements are present in the list using the `LRANGE` command.

```bash
> LRANGE list_key 0 -1
# Expect RESP Encoded Array: ["two", "three", "four", "five"]
``` 