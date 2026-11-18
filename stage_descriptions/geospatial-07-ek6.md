Your server can decode a location's coordinates. In this stage, you'll use those
decoded positions to calculate the distance between two stored members with
`GEODIST`.

### The `GEODIST` command

The `GEODIST` command returns the distance between two members of a key.

Example usage:

```bash
> GEODIST places Munich Paris
"682477.7582"
```

The distance is returned in meters, encoded as a
[RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

Redis uses the
[haversine formula](https://rosettacode.org/wiki/Haversine_formula) to
approximate distance along Earth's surface as a sphere. Use the coordinates
decoded from the stored scores, converting degrees to radians for the
trigonometric functions. The
[Redis distance calculation](https://github.com/redis/redis/blob/4322cebc1764d433b3fce3b3a108252648bf59e7/src/geohash_helper.c#L228C1-L228C72)
provides a reference.

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

The tester will validate that the response is a RESP bulk string that contains
the distance between the two locations, for example:

```bash
$11\r\n682477.7582\r\n
```

### Notes

- When computing distance, use
  [Redis's Earth-radius constant](https://github.com/redis/redis/blob/35aacdf80a0871c933047fc46655b98a73a9374e/src/geohash_helper.c#L52)
  of 6372797.560856 meters.

- The tester allows an absolute difference of up to `0.0001` meters from the
  expected calculation. For example, if the expected distance is `12345.6789`,
  these values are accepted:
  - `12345.67891`
  - `12345.6789`
  - `12345.67889`
