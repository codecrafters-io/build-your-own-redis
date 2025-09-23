In this stage, you'll add support for removing the first element of a list using the `LPOP` command.

### The `LPOP` Command

The [`LPOP`](https://redis.io/docs/latest/commands/lpop/) command removes and returns the first element of a list.

Here's an example:

```bash
> RPUSH list_key "a" "b" "c" "d"
(integer) 4

> LPOP list_key
"a"
```

If the list is empty or doesn't exist, it returns a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`).

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

Next, it will send an `LPOP` command to your server specifying the list that was just created:

```bash
> LPOP list_key
# Expecting: (Bulk string) "one"
```

The tester will expect the removed element to be returned as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings) (`$3\r\none\r\n`).

The tester will also verify that the remaining elements are present in the list using the `LRANGE` command.

```bash
> LRANGE list_key 0 -1
# Expect RESP Encoded Array: ["two", "three", "four", "five"]
``` 
