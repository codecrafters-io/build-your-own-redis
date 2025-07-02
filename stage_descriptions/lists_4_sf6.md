In this stage, you will add support for listing the elements of a list using the `LRANGE` command.

### The `LRANGE` command

The `LRANGE` command is used to list the elements in a list given a start index and end index. The index of the first element 0. The end index is inclusive, which means that the element at the end index will be included in the response.

Example usage:

```bash
# Create a list with 5 items
> RPUSH list_key "a" "b" "c" "d" "e"
(integer) 5

# List first 2 items 
> LRANGE list_key 0 1
1) "a"
2) "b"

# List items from indexes 2-4
> LRANGE list_key 2 4
1) "c"
2) "d"
3) "e"
```

Here are some additional notes on how the LRANGE command behaves with different types of inputs:

- If the list does not exist, an empty array is returned
- If the start index is greater than or equal to the list's length, an empty array is returned.
- If the stop index is greater than the list's length, the stop index is treated as the last element.
- If the start index is greater than the stop index, the result is an empty array.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then create a new list with multiple elements.

```bash
$ redis-cli RPUSH list_key "a" "b" "c" "d" "e"
```

After that the tester will send your program a series of `LRANGE` commands. It will expect the response to be an RESP Array or empty array in each case, depending on the test case.

As an example, the tester might send your program a command like this:

```bash
$ redis-cli LRANGE list_key 0 2
# Expect RESP Encoded Array: ["a", "b", "c"]
```

It will expect the response to be an RESP-encoded array `["a", "b", "c"]`, which would look like this:

```bash
*3\r\n
$1\r\n
a\r\n
$1\r\n
b\r\n
$1\r\n
c\r\n
```

The tester will issue multiple such commands and verify their responses.

### Notes

- In this stage, you will only implement `LRANGE` with non-negative indexes. We will get to handling `LRANGE` for negative indexes in the next stage.
- If a list doesn't exist, `LRANGE` should respond with an empty RESP array (`*0\r\n`). 