In this stage, you'll add support for validating the latitude and longitude provided in the `GEOADD` command.

### The `GEOADD` command (Validate coordinates)
Latitudes and longitudes used in the `GEOADD` command should be in a certain range as per [EPSG:3857](https://epsg.io/3857). Valid longitudes are from -180 to 180 degrees. Valid latitudes are from -85.05112878 to 85.05112878 degrees. Both of these limits are inclusive.

If either of these values aren't within the appropriate range, `GEOADD` returns an error. Examples:

```bash
# Invalid latitude
> GEOADD places 180 90 test1
(error) ERR invalid longitude,latitude pair 180.000000,90.000000

# Invalid longitude
> GEOADD places 181 0.3 test2
(error) ERR invalid longitude,latitude pair 181.000000,0.300000
```

### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send multiple `GEOADD` commands. If the coordinates supplied are valid, the tester will expect the response to be `:1\r\n`, the usual response of the `GEOADD` command. For invalid coordinates values, it will expect an error response.

For example, the tester might send your program a command like this:

```bash
$ redis-cli GEOADD location_key 200 100 foo

# Expecting error
(error) ERR invalid longitude,latitude pair 200,100
```

The value is RESP simple error, which is RESP-encoded as

```bash
-ERR invalid longitude,latitude pair 200,100\r\n
```

### Notes

- In this stage, you'll only extend responding to `GEOADD` in case of invalid values of latitude and longitude. You do not need to implement the storage mechanism yet.

- The tester is lenient in checking error messages, it doesn't require that errors match Redis's exact format.
    - The error response must:
        - Start with "ERR invalid" (case-insensitive)
        - Include "latitude" if latitude is invalid.
        - Include "longitude" if longitude is invalid.

    - Examples:
        - ✅ ERR invalid latitude value
        - ❌ ERR invalid

- The boundary of latitude are clipped at -85.05112878 and 85.05112878 degrees instead of -90 and 90 degrees respectively. It is because of the Web Mercator projection used by Redis. You can read more about it on [Wikipedia](https://en.wikipedia.org/wiki/Web_Mercator_projection).