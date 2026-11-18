This extension adds
[sorted sets](https://redis.io/docs/latest/develop/data-types/sorted-sets/) so
clients can query members by their position in score order, as in a leaderboard.
In this stage, you'll create a sorted set with one member using `ZADD`.

### Redis sorted sets

Sorted sets are one of the data types that Redis supports. A sorted set is a
collection of unique elements, where each element is associated with a
floating-point score. Unlike regular sets, sorted sets maintain elements in a
defined order based on their scores.

This makes them useful for use cases like leaderboards, priority queues, or any
scenario where you need fast access to items sorted by a numerical value.

For example, if you were using sorted sets to store user rankings in a game
leaderboard, the contents of the sorted set might look like this:

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

### The `ZADD` command

The [ZADD](https://redis.io/docs/latest/commands/zadd/) command is used to add a
member to a sorted set.

If the sorted set does not exist, it is created and the member is added to it.

Example usage:

```bash
> ZADD racer_scores 8.0 "Sam"
(integer) 1
```

The arguments are the key, score, and member name, in that order. The response
is the number of new members added. RESP encodes an integer as `:<number>\r\n`,
so adding the first member returns `:1\r\n`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `ZADD` command specifying a key, score, and member name.

```bash
$ redis-cli ZADD zset_key 10.0 zset_member
```

The tester will verify that the response to the command is `:1\r\n`, which is 1
(the number of members added to the sorted set), encoded as a RESP Integer.

### Notes

- In this stage, you'll only need to handle creating a new sorted set with a
  single member. We will get to adding new members to existing sorted set in the
  later stages.
- Redis uses a 64-bit floating-point
  [`double`](https://github.com/redis/redis/blob/bec644aab198049eaa5583631c419b4574b137e1/tests/modules/zset.c#L34)
  for scores. Use a representation with compatible precision.

- For reference, one of Redis's sorted-set representations combines a
  [hash table and skip list](https://github.com/redis/redis/blob/674b829981c0b8ad15a670a32df503e0e4514e96/src/server.h#L1560).
  This challenge doesn't require that internal structure.
