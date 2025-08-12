In this stage, you'll add support for creating a [sorted set](https://redis.io/docs/latest/develop/data-types/sorted-sets/) using the `ZADD` command.

### Redis Sorted Sets

Sorted sets are one of the data types that Redis supports. A sorted set is a collection of unique elements, where each element is associated with a floating-point score. Unlike regular sets, sorted sets maintain elements in a defined order based on their scores.

This makes them useful for use cases like leaderboards, priority queues, or any scenario where you need fast access to items sorted by a numerical value.

For example, if you were using sorted sets to store user rankings in a game leaderboard, the contents of the sorted set might look like this:

```yaml
racer_scores:
    - member: "Ford"
      score: 6.1
    - member: "Royce"
      score: 8.2
    - member: "Sam-Bodden"
      score: 8.2
    - member: "Prickett"
      score: 14.5
```

Sorted sets are ordered based on increasing scores.


### The `ZADD` Command

The [ZADD](https://redis.io/docs/latest/commands/zadd/) command is used to add a member to a sorted set.

If the sorted set does not exist, it is created and the member is added to it.

Example usage:

```bash
> ZADD racer_scores 8.0 "Sam"
(integer) 1
```

The `ZADD` command takes the key, a score, and the member name as arguments. It returns an integer representing the number of new members added to the sorted set.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command specifying a key, value and score.

```bash
$ redis-cli ZADD zset_key 10.0 zset_member
```

The tester will verify that the response to the command is `:1\r\n`, which is 1 (the number of members added to the sorted set), encoded as a RESP Integer.

### Notes
- In this stage, you'll only need to handle creating a new sorted set with a single member. We will get to adding new members to existing sorted set in the later stages.
- It is recommended to store the score as a 64 bit floating point number for highest precision as the official Redis implementation uses [`double`](https://github.com/redis/redis/blob/bec644aab198049eaa5583631c419b4574b137e1/tests/modules/zset.c#L34).

- We suggest that you implement sorted sets using a data structure where the members are stored in a sorted fashion according to their scores. It'll come in handy in the later stages.
    -  Redis implements sorted sets using a combination of [hash table and skip list](https://github.com/redis/redis/blob/674b829981c0b8ad15a670a32df503e0e4514e96/src/server.h#L1560).