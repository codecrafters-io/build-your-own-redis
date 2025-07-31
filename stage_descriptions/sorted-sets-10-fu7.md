In this stage, you'll add support for removing multiple zset members using the `ZREM` command.

## The `ZREM` command (Removing multiple members)

The `ZREM` command can be used to remove multiple members from a zset.

Example Usage:

```bash
> ZADD racer_scores 8 "Sam-Bodden"
(integer) 1
> ZADD racer_scores 10 "Royce"
(integer) 1
> ZADD racer_scores 6 "Ford"
(integer) 1
> ZADD racer_scores 14 "Prickett"
(integer) 1

# Remove "Royce" from the zset
> ZREM racer_scores "Royce" "Prickett"
(integer) 2

# List the remaining members
> ZRANGE racer_scores 0 -1
1) "Ford"
2) "Sam-Bodden"
```


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

After that the tester will send your program a `ZREM` command specifying multiple members to be removed.

As an example, the tester might send your program a command like this.
```bash
> ZREM zset_key "member3" "foo" "member1"
# Expected: (integer) 3
```

It will then send your program a `ZRANGE` command and check for the remaining members.

```bash
> ZRANGE zset_key 0 -1
# Expected RESP Encoded Array: ["member2", "member4", "bar"]
```