In this stage, you'll add support for responding to the `GEOPOS` command.

### The `GEOPOS` command

The `GEOPOS` command returns the longitude and latitude of the specified location.

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

It returns an array with one entry for each location requested.

- If no locations are specified, an empty array is returned `(*0\r\n)`
- If a location exists under the key, its entry is an array of two items:
  - Longitude (Encoded as a [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings))
  - Latitude (Encoded as a [RESP Bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings))
- If a location doesn’t exist, the entry is a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) `($-1\r\n)`.
- If the key does not exist:
  - If no locations are specified, an empty array is returned `(*0\r\n)`.
  - For each location specified, a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) `($-1\r\n)` is returned.


To return the latitude and longitude values, Redis decodes the "score" back to latitude and longitude values. We'll cover this process in later stages, for now you can hardcode the returned latitude and longitude values to be 0 (or any number).

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

The tester will then send multiple `GEOPOS` commands:

```bash
> GEOPOS location_key London Munich
# Expecting: [["0", "0"], ["0", "0"]], encoded as "*2\r\n$1\r\n0\r\n$1\r\n0\r\n$1\r\n0\r\n$1\r\n0\r\n"
> GEOPOS location_key missing_location
# Expecting: [nil], encoded as "*1\r\n$-1\r\n"
> GEOPOS location_key
# Expecting: [] encoded as "*0\r\n"
```

The tester will assert that:

- The response is a RESP array that contains as many elements as the number of locations requested
- For each location requested:
  - If the location exists:
    - The corresponding element is a RESP array with two elements (i.e. longitude and latitude)
    - Both elements are "0" (or any other valid floating point number), encoded as a RESP bulk string
  - If the location doesn't exist:
    - The corresponding element is a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) `($-1\r\n)`.

The tester will also send a `GEOPOS` command using a key that doesn't exist:

```bash
> GEOPOS missing_key
# Expecting: [], encoded as "*0\r\n"

> GEOPOS missing_key location
# Expecting [nil], encoded as "*1\r\n$-1\r\n"
```

The tester will assert that:
- The response is a RESP array that contains as many null bulk string (`$-1\r\n`) as the number of locations requested

### Notes

- In this stage, you can hardcode the returned latitude and longitude values to be 0 (or any other valid floating point number). We'll get to testing actual latitude and longitude values in later stages.
