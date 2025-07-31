In this stage, you'll add support for listing the members of a zset using the `ZRANGE` command.

### The `ZRANGE` command
The `ZRANGE` command is used to list the members in a zset given a start index and an end index. The index of the first element is 0. The end index is inclusive, which means that the element at the end index will be included in the response.

Example usage:
```bash
> ZADD racer_scores 8 "Sam-Bodden"
(integer) 1
> ZADD racer_scores 10 "Royce"
(integer) 1
> ZADD racer_scores 6 "Ford"
(integer) 1
> ZADD racer_scores 14 "Prickett"
(integer) 1

# List members from index 0 to 2
> ZRANGE racer_scores 0 2
1) "Ford"
2) "Sam-Bodden"
3) "Royce"
```

Here are some additional notes on how the `ZRANGE` command behaves with different types of inputs:

- If the zset does not exist, an empty array is returned
- If the start index is greater than or equal to the cardinality of the zset, an empty array is returned.
- If the stop index is greater than the cardinality of the zset, the stop index is treated as the last element.
- If the start index is greater than the stop index, the result is an empty array.


### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then create a new zset with multiple members.

```bash
$ redis-cli
> ZADD zset_key 20.0 member1 (Expecting ":1\r\n")
> ZADD zset_key 30.1 member2 (Expecting ":1\r\n")
> ZADD zset_key 100.0 foo (Expecting ":1\r\n")
> ZADD zset_key 100.0 bar (Expecting ":1\r\n")
> ZADD zset_key 40.2 member3 (Expecting ":1\r\n")
> ZADD zset_key 50.3 member4 (Expecting ":1\r\n")
```

After that the tester will send your program a series of `ZRANGE` commands. It will expect the response to be a RESP array, or an empty array in each case, depending on the test case.

As an example, the tester might send your program a command like this.
```bash
> ZRANGE zset_key 3 5
# Expect RESP Encoded Array: ["member4", "bar", "foo"]
```

It will expect the response to be an RESP-encoded array `["member4", "bar", "foo"]`, which would look like this:
```
*3\r\n
$7\r\n
member4\r\n
$3\r\n
bar\r\n
$3\r\n
foo\r\n
```

The tester will issue multiple such commands and verify their responses.

### Notes

- In this stage, you will only implement `ZRANGE` with non-negative indexes. We will get to handling `ZRANGE` for negative indexes, and the implementation of `REV` option in the later stages.