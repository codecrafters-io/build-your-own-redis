In this stage, you'll add support for querying the length of a list using the `LLEN` command.

### The `LLEN` Command

The [`LLEN`](https://redis.io/docs/latest/commands/llen/) command is used to get the number of elements in a list. This number is returned as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

For example:

```bash
# Create a new list with 4 items
> RPUSH list_key "a" "b" "c" "d"
(integer) 4

# Get the length of the new list
> LLEN list_key
(integer) 4
```

If the list doesn't exist, the server returns `0` as a RESP integer.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then create a list with a random number of elements using `RPUSH`.

```bash
$ redis-cli
> RPUSH list_key <random number of elements>
```

Next, it will send a `LLEN` command for the newly created list. 

```bash
> LLEN list_key
# Expect: list_length (RESP Encoded Integer)
```

Your program is expected to respond with the length of the list, encoded as a RESP integer.

The tester will also verify the response for a non-existent list:

```bash
> LLEN missing_list_key
# Expect:  (integer) 0
```

In this case, your program should respond with `0`, which is encoded as `:0\r\n`.

### Notes
- `LLEN` should work with lists created by both `RPUSH` and `LPUSH` commands.
