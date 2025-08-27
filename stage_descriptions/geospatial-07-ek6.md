In this stage, you'll add support for calculating the distance between two locations using the `GEODIST` command.

### The `GEODIST` command

The `GEODIST` command returns the distance between two members of a key.

Example usage:

```bash
> GEODIST places Munich Paris
"682477.7582"
```

The distance is returned in meters, encoded as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

Redis uses the [Haversine's Formula](https://en.wikipedia.org/wiki/Haversine_formula#Example) to calculate the distance between two points. Please note that the linked example on Wikipedia uses degree as the unit of arguments for `sin()` and `cos()`. Most math libraries use radian as the unit for trigonometric functions like `sin()` and `cos()`. So, make sure to make appropriate conversions wherever necessary.

You can also see how the distance calculation is implemented in the Redis source code [here](https://github.com/redis/redis/blob/4322cebc1764d433b3fce3b3a108252648bf59e7/src/geohash_helper.c#L228C1-L228C72).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then add multiple locations using the `GEOADD` command:

```bash
$ redis-cli
> GEOADD places 11.5030378 48.164271 "Munich"
> GEOADD places 2.2944692 48.8584625 "Paris"
```

The tester will then send multiple `GEODIST` commands specifying two locations:

```bash
> GEODIST places Munich Paris
# Expecting "682477.7582"
```

The tester will validate that the response is a RESP bulk string that contains the distance between the two locations, for example:

```bash
$11\r\n682477.7582\r\n
```

### Notes

- For the maximum precision, please keep set the value of Earth radius's to 6372797.560856 meters. This is is the exact value used by [Redis](https://github.com/redis/redis/blob/35aacdf80a0871c933047fc46655b98a73a9374e/src/geohash_helper.c#L52), and also the tester.

- The distance should match the actual distance between the provided locations with a precision of **up to 2 decimal places after rounding**. For example, if the expected distance is `12345.67`, any of the following returned values will be accepted:
  - `12345.67001`
  - `12345.674`
  - `12345.6659`
  - `12345.6666`
  - `12345.669`
