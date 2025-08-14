In this stage, you'll add support for counting the number of members in a sorted set using the `ZCARD` command.

### The `ZCARD` Command

The `ZCARD` command is used to query the cardinality (number of elements) of a sorted set. It returns an integer. The response is 0 if the sorted set specified does not exist.

```bash
> ZADD zset_key 1.2 "one"
(integer) 1
> ZADD zset_key 2.2 "two"
(integer) 1
> ZCARD zset_key
(integer) 2

> ZCARD missing_key
(integer) 0
```

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
> ZADD zset_key 50.3 zset_member4 (Expecting ":1\r\n")
```

It will check the number of elements in the sorted set using the `ZCARD` command.
```bash
$ redis-cli ZCARD zset_key (Expecting ":4\r\n")
```

The tester will then update the score of an existing member.
```bash
$ redis-cli ZADD zset_key 100.0 zset_member1 (Expecting ":0\r\n")
```

It will again check the cardinality of the sorted set using the `ZCARD` command.
```bash
$ redis-cli ZCARD zset_key (Expecting ":4\r\n")
```

The tester will also check the cardinality of a non existing sorted set.
```bash
$ redis-cli ZCARD missing_key (Expecting ":0\r\n")
```