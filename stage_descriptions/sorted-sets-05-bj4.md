In this stage, you'll add support for negative indexes for the `ZRANGE` command.

### `ZRANGE` with negative indexes

The `ZRANGE` command can accept negative indexes too.

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
> ZADD racer_scores 10 "Ben"
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

An index of -1 refers to the last element, -2 to the second last, and so on. If a absolute value of the negative index is out of range (i.e. >= the cardinality of the zset), it is treated as 0 (start of the zset).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command to create and add members to it.

```bash
$ redis-cli
> ZADD zset_key 20.0 zset_member1 (Expecting ":1\r\n")
> ZADD zset_key 30.1 zset_member2 (Expecting ":1\r\n")
> ZADD zset_key 40.2 zset_member3 (Expecting ":1\r\n")
> ZADD zset_key 25.0 foo (Expecting ":1\r\n")
> ZADD zset_key 25.0 bar (Expecting ":1\r\n")
```

The tester will then send your program a series of `ZRANGE` commands with one or more negative indexes.

For example, the tester might send you this command:

```bash
> ZRANGE zset_key 2 -1
1) "foo"
2) "zset_member2"
3) "zset_member3"
```

In this case, the tester will verify that the response is the array ["foo", "zset_member2", "zset_member3"], which is RESP Encoded as:

```
*3\r\n
$3\r\n
foo\r\n
$12\r\n
zset_member2\r\n
$12\r\n
zset_member3\r\n
```

### Notes

- In this stage, we'll only implement `ZRANGE` with no options. We'll get to the `REV` option in the next stage.