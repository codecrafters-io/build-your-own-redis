In this stage, you'll add support for retrieving the score of a sorted set member using the `ZSCORE` command.

### The `ZSCORE` Command

The `ZSCORE` command is used to query the score of a member of a sorted set. If the sorted set and the member both exist, the score of the member is returned as a RESP bulk string.
```bash
> ZADD zset_key 24.34 "one"
(integer) 1
> ZADD zset_key 90.34 "two"
(integer) 1
> ZSCORE zset_key "one"
"24.34"
```

If the member or the sorted set specified in the argument does not exist, RESP null bulk string is returned.
```bash
> ZSCORE zset_key "three"
(nil)
> ZSCORE missing_key "member"
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
> ZADD zset_key 20.0 zset_member1 (Expecting ":1\r\n")
> ZADD zset_key 30.1 zset_member2 (Expecting ":1\r\n")
> ZADD zset_key 40.2 zset_member3 (Expecting ":1\r\n")
> ZADD zset_key 50.3 zset_member4 (Expecting ":1\r\n")
```

The tester will then send a `ZSCORE` command specifying one of the members. For example, the tester may send your program a command like this:

```bash
> ZSCORE zset_key zset_member2 (Expecting RESP bulk string "30.1")
```

It will expect the response to be "30.1", which is the score of `zset_member2`. The response is encoded as a RESP bulk string:
```
$4\r\n
30.1\r\n
```

The tester will then update the value of the member.
```bash
> ZADD zset_key 100.99 zset_member2 (Expecting ":0\r\n")
```

The tester will then send a `ZSCORE` command specifying the updated member.

```bash
> ZSCORE zset_key zset_member2 (Expecting RESP bulk string "100.99")
```

The tester will also send `ZSCORE` command specifying a non-existent member, and a non-existent key.
```bash
> ZSCORE zset_key zset_member100 (Expecting RESP bulk string "$-1\r\n")
> ZSCORE missing_key member (Expecting RESP bulk string "$-1\r\n")
```