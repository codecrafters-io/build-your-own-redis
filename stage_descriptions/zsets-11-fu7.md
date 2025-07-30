In this stage, you'll add support for removing multiple elements in a single `ZPOPMIN` command.

### `ZPOPMIN` with multiple elements

The `ZPOPMIN` command accepts an optional argument to specify how many elements are to be removed.

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


# Removes the two member with lowest score from the zset
> ZPOPMIN racer_scores 2
1) "Ford"
2) "6"
3) "Sam-Bodden"
4) "8"
```

In case of removing multiple elements, an array is returned where each pair represents a member and its score:
- the first item is the member, the second is its score, the third is the next member, and so on.

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
> ZPOPMIN zset_key 2
# Expecting RESP Encoded Array: ["zset_member1", "20.0", "bar", "25.0"]
```

The tester will also verify that the remaining members are present in the zset using the `ZRANGE` command.

```bash
> ZRANGE zset_key 0 -1
# Expect RESP Encoded Array: ["foo", "zset_member2", "zset_member3"]
``` 