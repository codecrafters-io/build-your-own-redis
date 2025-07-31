In this stage, you'll add support for retrieving the rank of a member in a zset.

### The `ZRANK` Command

The `ZRANK` command is used to query the rank of a member in a zset. It returns an integer, which is 0-based index of the member when the sorted set is ordered by increasing score.
If two members have same score, the members are ordered lexicographically. If the member, or the zset does not exist, the command returns null bulk string.

Example usage:
```bash
> ZADD zset_key 1.0 one
(integer) 1
> ZADD zset_key 2.0 two
(integer) 1
> ZADD zset_key 2.0 three
(integer) 1


> ZRANK zset_key one
(integer) 0
> ZRANK zset_key two
(integer) 2
> ZRANK zset_key three
(integer) 1

# Missing zset and member
> ZRANK zset_key missing
(nil)
> ZRANK non_existent_key member
(nil)
```

The rank of `three` is 1, and `two` is 2. It is because though the both members have same scores, "three" preceeds "two" lexicographically.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command to create and add new members to it.

```bash
$ redis-cli
> ZADD zset_key 20.0 zset_member1 (Expecting ":1\r\n")
> ZADD zset_key 30.1 zset_member2 (Expecting ":1\r\n")
> ZADD zset_key 40.2 zset_member3 (Expecting ":1\r\n")
> ZADD zset_key 50.3 zset_member4 (Expecting ":1\r\n")

> ZADD zset_key 100.0 foo (Expecting ":1\r\n")
> ZADD zset_key 100.0 bar (Expecting ":1\r\n")

# Expected Ranks
# zset_member1 -> 0
# zset_member2 -> 1
# zset_member3 -> 2
# zset_member4 -> 3
# foo -> 5
# bar -> 4
```

The tester will then send multiple `ZRANK` commands specifying the members of the zset.
```bash
> ZRANK zset_key zset_member3 (Expecting ":2\r\n")
> ZRANK zset_key zset_member1 (Expecting ":0\r\n")
> ZRANK zset_key foo (Expecting ":5\r\n")
> ZRANK zset_key bar (Expecting ":4\r\n")
```

The tester will also send `ZRANK` command specifying a non-existing member, and a non-existent zset.

```bash
> ZRANK zset_key zset_member100 (Expecting RESP bulk string "$-1\r\n")
> ZRANK non_existent_key member (Expecting RESP bulk string "$-1\r\n")
```