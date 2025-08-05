In this stage, you'll add support for adding elements to an existing sorted set.

### Adding elements using `ZADD`

The `ZADD` command can be used to add a new member to an existing sorted set, or update the score of an existing member. It returns the count of new members added to the sorted set as an integer.


Example usage:
```bash
> ZADD zset_key 0.0043 foo
(integer) 1
> ZADD zset_key 8.0 bar
(integer) 1

# No new members were added
> ZADD zset_key 10.0 bar
(integer) 0
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command to create a new sorted set.

```bash
$ redis-cli ZADD zset_key 20.0 zset_member1 (Expecting ":1\r\n")
```

The tester will then send the `ZADD` command a few times to add new members.

```bash
$ redis-cli
> ZADD zset_key 30.1 zset_member2 (Expecting ":1\r\n")
> ZADD zset_key 40.2 zset_member3 (Expecting ":1\r\n")
> ZADD zset_key 50.3 zset_member4 (Expecting ":1\r\n")
```

The tester expects the response to be `:1\r\n` whenever a new member is added.

It will then update the score of an existing member.
```bash
> ZADD zset_key 100.0 zset_member1 (Expecting ":0\r\n")
```

The tester expects the response to be `:0\r\n` whenever an existing member is updated.