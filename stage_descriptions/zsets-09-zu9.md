In this stage, you'll add support for `REV` option for the `ZRANGE` command.

### `REV` option in `ZRANGE`

The `REV` option in the `ZRANGE` can be used to list the members in the reverse order.

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


# List all elements in reverse order
> LRANGE racer_scores 0 -1 REV
1) "Prickett"
2) "Royce"
3) "Ben"
4) "Sam-Bodden"
5) "Ford"
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
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

The tester will then send your program a series of `ZRANGE` commands specifying the `REV` option.

For example, the tester might send you this command:

```
> ZRANGE zset_key 0 -1 REV
1) "zset_member3"
2) "zset_member2"
3) "foo"
4) "bar"
5) "zset_member1"
```

In this case, the response is a an array, which is RESP Encoded as:

```
*5\r\n
$12\r\n
zset_member3\r\n
$12\r\n
zset_member2\r\n
$3\r\n
foo\r\n
$3\r\n
bar\r\n
$12\r\n
zset_member1\r\n
```