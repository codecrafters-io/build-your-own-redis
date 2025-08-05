In this stage, you'll add support for retrieving the rank of a sorted set member.

### The `ZRANK` Command

The `ZRANK` command is used to query the rank of a member in a sorted set. It returns an integer, which is 0-based index of the member when the sorted set is ordered by increasing score.
If two members have same score, the members are ordered lexicographically.

Example usage:
```bash
> ZADD zset_key 1.0 member_with_score_1
(integer) 1
> ZADD zset_key 2.0 member_with_score_2
(integer) 1
> ZADD zset_key 2.0 another_member_with_score_2
(integer) 1


> ZRANK zset_key member_with_score_1
(integer) 0
> ZRANK zset_key member_with_score_2
(integer) 2
> ZRANK zset_key another_member_with_score_2
(integer) 1
```

The rank of `another_member_with_score_2` is 1, and `member_with_score_2` is 2. It is because though the both members have same scores, `another_member_with_score_2` preceeds `member_with_score_2` lexicographically.


If the member, or the sorted set does not exist, the command returns null bulk string.
```bash
# Missing sorted set and member
> ZRANK zset_key missing_member
(nil)
> ZRANK missing_key member
(nil)
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command to create and add new members to it.

```bash
$ redis-cli
> ZADD zset_key 100.0 foo (Expecting ":1\r\n")
> ZADD zset_key 100.0 bar (Expecting ":1\r\n")
> ZADD zset_key 20.0 baz (Expecting ":1\r\n")
> ZADD zset_key 30.1 caz (Expecting ":1\r\n")
> ZADD zset_key 40.2 paz (Expecting ":1\r\n")

# Expected Ranks
# baz -> 0
# caz -> 1
# paz -> 2
# bar -> 3
# foo -> 4
```

The tester will then send multiple `ZRANK` commands specifying the members of the sorted set.
```bash
> ZRANK zset_key caz (Expecting ":1\r\n")
> ZRANK zset_key baz (Expecting ":0\r\n")
> ZRANK zset_key foo (Expecting ":4\r\n")
> ZRANK zset_key bar (Expecting ":3\r\n")
```

The tester will also send `ZRANK` command specifying a non-existing member, and a non-existent sorted set.

```bash
> ZRANK zset_key missing_member (Expecting RESP bulk string "$-1\r\n")
> ZRANK missing_key member (Expecting RESP bulk string "$-1\r\n")
```