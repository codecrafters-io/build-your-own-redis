In this stage, you'll add support for calculating the distance between two locations using the `GEODIST` comamnd.

### The `GEODIST` command
The `GEODIST` command returns the distance between two members of a key. The default unit of the distance which is returned, is meters.

Example usage:

```bash
> GEODIST places Munich Paris
"682477.7582"
```

It returns the distance as a string, encoded as a RESP Bulk String.

Redis uses the [Haversine's Formula](https://en.wikipedia.org/wiki/Haversine_formula#Example) to calculate the distance between two points. You can see how this is done in the Redis source code [here](https://github.com/redis/redis/blob/4322cebc1764d433b3fce3b3a108252648bf59e7/src/geohash_helper.c#L228C1-L228C72).

### Tests
The tester will execute your program like this:
```bash
$ ./your_program.sh
```

It will add multiple locations using the `GEOADD` command.
```bash
$ redis-cli
> GEOADD places 11.5030378 48.164271 "Munich"
> GEOADD places 2.2944692 48.8584625 "Paris"
```

The tester will then send multiple `GEODIST` commands specifying two locations. For example, the tester might send your program a command like this:

```bash
> GEODIST places Munich Paris
# Expecting "682477.7582"
```

The value is a RESP bulk string encoded as:

```
$11\r\n
682477.7582\r\n
```

### Notes
- The tester will be lenient when validating the distance returned by the `GEODIST` command. The distance should match the actual distance between the provided locations with a precision of **up to 2 decimal places after rounding**.

  * This means that minor floating-point differences are acceptable as long as the values, when rounded to two decimal places, are the same.

  * For example, if the expected distance is `12345.67`, any of the following returned values will be accepted:

    * `12345.67001`
    * `12345.674`
    * `12345.6659`
    * `12345.6666`
    * `12345.669`

  * However, values like `12345.64`, `12345.70`, or `12345.61` would be considered incorrect.

- If one or both of the location specified in the `GEODIST` command does not exist, it should return a null bulk string `($-1\r\n)`.