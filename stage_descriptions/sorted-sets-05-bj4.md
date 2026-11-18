Your `ZRANGE` counts positions from the beginning. In this stage, you'll support
negative indexes so clients can select members relative to the end of score
order without first querying the set's size.

### `ZRANGE` with negative indexes

The `ZRANGE` command can accept negative indexes too.

Example usage:

```bash
> ZADD racer_scores 8.5 "Sam-Bodden"
(integer) 1
> ZADD racer_scores 10.2 "Royce"
(integer) 1
> ZADD racer_scores 6.1 "Ford"
(integer) 1
> ZADD racer_scores 14.9 "Prickett"
(integer) 1
> ZADD racer_scores 10.2 "Ben"
(integer) 1


# List last 2 elements
> ZRANGE racer_scores -2 -1
1) "Royce"
2) "Prickett"

# List all items except last 2
> ZRANGE racer_scores 0 -3
1) "Ford"
2) "Sam-Bodden"
3) "Ben"
```

An index of `-1` refers to the last element, `-2` to the second last, and so on.
If a negative `start` points before the first member, treat it as `0`. If a
negative `stop` points before the first member, return an empty array. For the
five-member set above, `ZRANGE racer_scores -6 -1` returns all five members,
while `ZRANGE racer_scores 0 -6` returns an empty array.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command to create a sorted set and add members to it.

```bash
$ redis-cli
> ZADD zset_key 20.0 foo (Expecting ":1\r\n")
> ZADD zset_key 30.1 bar (Expecting ":1\r\n")
> ZADD zset_key 40.2 baz (Expecting ":1\r\n")
> ZADD zset_key 25.0 paz (Expecting ":1\r\n")
> ZADD zset_key 25.0 caz (Expecting ":1\r\n")
```

The tester will then send your program a series of `ZRANGE` commands with one or
more negative indexes.

For example, the tester might send you this command:

```bash
> ZRANGE zset_key 2 -1
1) "paz"
2) "bar"
3) "baz"
```

In this case, the tester will verify that the response is the array
`["paz", "bar", "baz"]`, which is RESP Encoded as:

```
*3\r\n
$3\r\n
paz\r\n
$3\r\n
bar\r\n
$3\r\n
baz\r\n
```
