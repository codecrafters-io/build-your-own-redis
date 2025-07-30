In this stage, you'll implement support for removing a single member with the lowest score using the `ZPOPMIN` command.

### The `ZPOPMIN` Command

The `ZPOPMIN` command removes the member with lowest score from the zset. The return value is an array of two elements:
1. Removed member (Encoded as a bulk string)
2. Score of the removed member (Encoded as a bulk string)

If the zset doesn't exist, empty array is returned.

Example usage:

```bash
> ZADD racer_scores 8 "Sam-Bodden"
(integer) 1
> ZADD racer_scores 10 "Royce"
(integer) 1
> ZADD racer_scores 6 "Ford"
(integer) 1
> ZADD racer_scores 14 "Prickett"
(integer) 1
> ZADD racer_scores 10 "Ben"
(integer) 1


# Removes the member with lowest score from the zset
> ZPOPMIN racer_scores
1) "Ford"
2) "6"

# Non existing zset
> ZPOPMIN non_existent
(empty array)
```

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send a `ZADD` command to create and add members to it.

```bash
$ redis-cli
> ZADD zset_key 20.0 zset_member1 (Expecting ":1\r\n")
> ZADD zset_key 30.1 zset_member2 (Expecting ":1\r\n")
> ZADD zset_key 40.2 zset_member3 (Expecting ":1\r\n")
> ZADD zset_key 25.0 foo (Expecting ":1\r\n")
> ZADD zset_key 25.0 bar (Expecting ":1\r\n")
```

It will then send an `ZPOPMIN` command to your program specifying the zset that was just created.

```bash
> ZPOPMIN zset_key
# Expecting RESP Encoded Array: ["zset_member1", "20"]
```

The tester will also verify that the remaining members are present in the zset using the `ZRANGE` command.

```bash
> ZRANGE zset_key 0 -1
# Expect RESP Encoded Array: ["bar", "foo", "zset_member2", "zset_member3"]
``` 