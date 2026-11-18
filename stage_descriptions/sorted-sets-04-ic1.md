`ZRANK` reports one member's position. In this stage, you'll add `ZRANGE` to
return the members occupying a range of positions, using the same score and name
ordering.

### The `ZRANGE` command

`ZRANGE` takes a key, a start index, and a stop index. Index `0` is the first
member, and both boundaries are inclusive. Return the selected member names as a
RESP array of bulk strings. Scores aren't included in this form of the command.

Example usage:

```bash
> ZADD racer_scores 8.1 "Sam-Bodden"
(integer) 1
> ZADD racer_scores 10.2 "Royce"
(integer) 1
> ZADD racer_scores 6.0 "Ford"
(integer) 1
> ZADD racer_scores 14.1 "Prickett"
(integer) 1

# List members from index 0 to 2
> ZRANGE racer_scores 0 2
1) "Ford"
2) "Sam-Bodden"
3) "Royce"
```

Here are some additional notes on how the `ZRANGE` command behaves with
different types of inputs:

- If the sorted set does not exist, an empty array (`*0\r\n`) is returned
- If the start index is greater than or equal to the number of members, an empty
  array is returned.
- If the stop index is greater than or equal to the number of members, treat it
  as the last member's index.
- If the start index is greater than the stop index, the result is an empty
  array.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then create a new sorted set with multiple members.

```bash
$ redis-cli
> ZADD zset_key 100.0 foo (Expecting ":1\r\n")
> ZADD zset_key 100.0 bar (Expecting ":1\r\n")
> ZADD zset_key 20.0 baz (Expecting ":1\r\n")
> ZADD zset_key 30.1 caz (Expecting ":1\r\n")
> ZADD zset_key 40.2 paz (Expecting ":1\r\n")
```

After that the tester will send your program a series of `ZRANGE` commands. It
will expect the response to be a RESP array, or an empty array in each case,
depending on the test case.

As an example, the tester might send your program a command like this.

```bash
> ZRANGE zset_key 2 4
# Expect RESP Encoded Array: ["paz", "bar", "foo"]
```

It will expect the response to be an RESP-encoded array `["paz", "bar", "foo"]`,
which would look like this:

```
*3\r\n
$3\r\n
paz\r\n
$3\r\n
bar\r\n
$3\r\n
foo\r\n
```

The tester will issue multiple such commands and verify their responses.

### Notes

- The response block splits RESP fragments across display lines. Keep the CRLF
  bytes represented by `\r\n` without adding those display line breaks.
- In this stage, you will only implement `ZRANGE` with non-negative indexes. We
  will get to handling `ZRANGE` with negative indexes in later stages.
