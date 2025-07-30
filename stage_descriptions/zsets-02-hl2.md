In this stage, you'll add support for verifying the value of the score argument for `ZADD` command.

### The `score` argument

The score argument of the `ZADD` command should be a valid floating point number.

If the value of score is not a valid floating point number, error is returned.

Example usage:

```bash
# Valid values of score
> ZADD zset_key 8.0 foo
(integer) 1
> ZADD zset_key -1e9 bar
(integer) 1
> ZADD zset_key 1e-9 baz
(integer) 1
> ZADD zset_key 0.0043 caz
(integer) 1
> ZADD zset_key 100 paz
(integer) 1

# Invalid values of score
> ZADD zset_key 0.x Foo
(error) ERR value is not a valid float
> ZADD zset_key score Bar
(error) ERR value is not a valid float
```


### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send multiple `ZADD` commands specifying different keys. The value of score may be valid or invalid. For example, the tester may send your program commands like these:

```bash
# Valid value of scores

$ redis-cli
> ZADD zset_key1 0.09 zset_member1 (Expecting ":1\r\n")
> ZADD zset_key2 20 zset_member2 (Expecting ":1\r\n")
> ZADD zset_key3 1e-9 zset_member3 (Expecting ":1\r\n")
> ZADD zset_key4 -2e10 zset_member4 (Expecting ":1\r\n")


# Invalid value of scores

> ZADD zset_key5 0.x zset_member5
# Expecting: (error) ERR value is not a valid float

ZADD zset_key6 score zset_member6
# Expecting: (error) ERR value is not a valid float
```

The error in case of invalid score is a simple RESP error, which is encoded as:
```
-ERR value is not a valid float\r\n
```

### Tests
- Redis stores the score value of zset members as a `double`, i.e. as a 64-bit floating point number. We suggest that you do the same for maximum precision.
- Redis uses [`strtod()`](https://man7.org/linux/man-pages/man3/strtod.3.html) function to convert the score argument from string to its value. You can see how it is done [here](https://github.com/redis/redis/blob/db4fc2a83309bf8b65e25deedfac0ff71d67e4b8/tests/modules/zset.c#L37). So, we recommend you use a function that is equivalent to the `strtod()` function available in your language.
