In this stage, you'll add support for creating a new sorted set using the `ZADD` command.

### The `ZADD` Command

The [ZADD](https://redis.io/docs/latest/commands/zadd/) command is used to add a member to a sorted set.

If the sorted set does not exist, it is created and the member is added to it. The syntax for `ZADD` is:

```
ZADD <zset_key> <score> <member>
```

The score is a floating point number.

 Example usage:

```bash
> ZADD racer_scores 8.0 "Sam"
(integer) 1
```

`ZADD` returns an integer, which is the number of members that were added to the zset.


Sorted set in Redis is a collection of unique strings ordered by their respective scores. It is also referred to as a zset. Sorted sets maintain a specific order based on the score of its members. You can read more about Redis sorted sets [here](https://redis.io/docs/latest/develop/data-types/sorted-sets/).


### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command specifying a key, value and score.

```bash
$ redis-cli ZADD zset_key 10.0 zset_member
```

The tester will verify that the response to the command is `:1\r\n`, which is 1 (the number of members added to the zset), encoded as a RESP Integer.

### Notes
- In this stage, you'll only need to handle creating a new zset with a single member. We will get to adding new members to existing zset in the later stages.
- It is recommended to store the score as a 64 bit floating point number for highest precision as the official Redis implementation uses [`double`](https://github.com/redis/redis/blob/bec644aab198049eaa5583631c419b4574b137e1/tests/modules/zset.c#L34).
