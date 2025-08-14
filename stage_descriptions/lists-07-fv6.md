In this stage, you'll add support for querying the length of a list using `LLEN`.

### The `LLEN` Command

The `LLEN` command is used to query a list's length. It returns a RESP-encoded integer.

Example usage:

```bash
> RPUSH list_key "a" "b" "c" "d"
(integer) 4

> LLEN list_key
(integer) 4
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then create a list with random number of elements using `RPUSH`.

```bash
$ redis-cli
> RPUSH list_key <random number of elements>
```

The tester will then send a `LLEN` command specifying the list that was just created.

```bash
> LLEN list_key
# Expect: list_length (RESP Encoded Integer)
```

It will expect the response to be length of the list encoded as a RESP integer.

It will also verify the response of `LLEN` command for a non-existent list.

```bash
> LLEN missing_list_key
# Expect:  (integer) 0
```

The tester expects 0, which is RESP Encoded as `:0\r\n `.
