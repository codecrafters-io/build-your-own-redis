In this stage, you'll add support for removing a single zset member using the `ZREM` command.

## The `ZREM` command

The [`ZREM`](https://redis.io/docs/latest/commands/zrem/) command is used to remove a member from a zset given the member's name.

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
> ZREM racer_scores "Royce"
(integer) 1

# List the remaining members
> ZRANGE racer_scores 0 -1
1) "Ford"
2) "Sam-Bodden"
3) "Prickett"

# Remove a non-existing member
> ZREM racer_scores "non_existing_member"
(integer)0
```

It returns the number of members removed from the zset. If the specified member does not exist, 0 is returned.

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

After that the tester will send your program a `ZREM` command specifying the member to be removed.

As an example, the tester might send your program a command like this.
```bash
> ZREM zset_key "member3"
# Expected: (integer) 1
```

It will then send your program a `ZRANGE` command and check for the remaining members.

```bash
> ZRANGE zset_key 0 -1
# Expected RESP Encoded Array: ["member1", "member2", "member4", "bar", "foo"]
```

The tester will also send a `ZREM` command specifying a non-existing member.
```bash
> ZREM zset_key "non_existent_member"
# Expected: (integer) 0
```

### Notes
- In this stage, you'll only need to remove one member at a time using the `ZREM` command. We'll get to removing multiple members in a single command in the next stage.