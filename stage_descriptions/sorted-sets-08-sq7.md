In this stage, you'll add support for removing a member of a sorted set using the `ZREM` command.

## The `ZREM` command

The [`ZREM`](https://redis.io/docs/latest/commands/zrem/) command is used to remove a member from a sorted set given the member's name.

Example Usage:

```bash
> ZADD racer_scores 8.3 "Sam-Bodden"
(integer) 1
> ZADD racer_scores 10.5 "Royce"
(integer) 1

# Remove "Royce" from the sorted set
> ZREM racer_scores "Royce"
(integer) 1

# List the remaining members
> ZRANGE racer_scores 0 -1
1) "Sam-Bodden"

# Remove a non-existing member
> ZREM racer_scores "missing_member"
(integer) 0
```

It returns the number of members removed from the sorted set. If the specified member does not exist, 0 is returned.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then create a new sorted set with multiple members.

```bash
$ redis-cli
> ZADD zset_key 80.5 foo (Expecting ":1\r\n")
> ZADD zset_key 50.3 baz (Expecting ":1\r\n")
> ZADD zset_key 80.5 bar (Expecting ":1\r\n")
```

After that the tester will send your program a `ZREM` command specifying the member to be removed.

As an example, the tester might send your program a command like this.
```bash
> ZREM zset_key "baz"
# Expected: (integer) 1
```

It will then send your program a `ZRANGE` command and check for the remaining members.

```bash
> ZRANGE zset_key 0 -1
# Expected RESP Encoded Array: ["bar", "foo"]
```

The tester will also send a `ZREM` command where the member doesn't exist.
```bash
> ZREM zset_key "missing_member"
# Expected: (integer) 0
```