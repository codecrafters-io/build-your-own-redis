Your server stores members and scores. In this stage, you'll implement `ZRANK`
to report a member's zero-based position when members are ordered by score.

### The `ZRANK` command

The `ZRANK` command is used to query the rank of a member in a sorted set. It
returns an integer, which is 0-based index of the member when the sorted set is
ordered by increasing score. If two members have the same score, compare their
names lexicographically by bytes. The first differing byte determines their
order, and a name that is a prefix of another comes first.

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

The two members with score `2.0` tie on score. `another_member_with_score_2`
sorts before `member_with_score_2`, giving them ranks `1` and `2` respectively.

If the member, or the sorted set does not exist, the command returns null bulk
string (`$-1\r\n`).

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

It will then use `ZADD` to create a sorted set and add members.

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

The tester will then send multiple `ZRANK` commands specifying the members of
the sorted set.

```bash
> ZRANK zset_key caz (Expecting ":1\r\n")
> ZRANK zset_key baz (Expecting ":0\r\n")
> ZRANK zset_key foo (Expecting ":4\r\n")
> ZRANK zset_key bar (Expecting ":3\r\n")
```

The tester will also send `ZRANK` commands where either the member or key
doesn't exist.

```bash
> ZRANK zset_key missing_member (Expecting RESP bulk string "$-1\r\n")
> ZRANK missing_key member (Expecting RESP bulk string "$-1\r\n")
```
