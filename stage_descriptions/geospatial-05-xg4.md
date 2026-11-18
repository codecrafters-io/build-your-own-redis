Your server stores names and location scores. In this stage, you'll add `GEOPOS`
to report which requested members exist using its nested response format.

### The `GEOPOS` command

The `GEOPOS` command returns the longitude and latitude of the specified
location.

Example usage:

```bash
> GEOADD places -0.0884948 51.506479 "London"
> GEOADD places 11.5030378 48.164271 "Munich"

> GEOPOS places London
1) 1) "-0.08849412202835083"
   2) "51.50647814139934"

> GEOPOS places Munich
1) 1) "11.503036916255951"
   2) "48.16427086232978"
```

It returns an array with one entry for each location requested, in the same
order as the request.

- If a location exists under the key, its entry is an array of two items:
  - Longitude (Encoded as a
    [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings))
  - Latitude (Encoded as a
    [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings))
- If either the location or key doesn't exist, the corresponding entry is a
  [null array](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays)
  `(*-1\r\n)`.

To return the latitude and longitude values, Redis decodes the "score" back to
latitude and longitude values. We'll cover this process in later stages, for now
you can hardcode the returned latitude and longitude values to be 0 (or any
number).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then add multiple locations using the `GEOADD` command.

```bash
$ redis-cli
> GEOADD location_key -0.0884948 51.506479 "London"
# Expect: (integer) 1
> GEOADD location_key 11.5030378 48.164271 "Munich"
# Expect: (integer) 1
```

The tester will then send `GEOPOS` commands for existing and missing locations.
It checks the response structure described above, accepting any valid
floating-point numbers as placeholder coordinates:

```bash
> GEOPOS location_key London Munich
# Expecting: [["0", "0"], ["0", "0"]], encoded as "*2\r\n*2\r\n$1\r\n0\r\n$1\r\n0\r\n*2\r\n$1\r\n0\r\n$1\r\n0\r\n"

> GEOPOS location_key missing_location
# Expecting: [nil], encoded as "*1\r\n*-1\r\n"
```

The tester will also query a key that doesn't exist:

```bash
> GEOPOS missing_key London Munich
# Expecting [nil, nil], encoded as "*2\r\n*-1\r\n*-1\r\n"
```
