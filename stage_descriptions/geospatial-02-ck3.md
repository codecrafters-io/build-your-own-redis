In this stage, you'll add support for validating the latitude and longitude values provided in a `GEOADD` command.

### Validating latitude and longitude values

The latitude and longitude values used in the `GEOADD` command should be in a certain range as per [EPSG:3857](https://epsg.io/3857).

- Valid longitudes are from -180° to +180°
  - Both these limits are inclusive, so -180° and +180° are both valid.
- Valid latitudes are from -85.05112878° to +85.05112878°
  - Both of these limits are inclusive, so -85.05112878° and +85.05112878° are both valid.
  - The reason these limits are not -/+90° is because of the [Web Mercator projection](https://en.wikipedia.org/wiki/Web_Mercator_projection) that Redis uses.

If either of these values aren't within the appropriate range, `GEOADD` returns an error. Examples:

```bash
# Invalid latitude
> GEOADD places 180 90 test1
(error) ERR invalid longitude,latitude pair 180.000000,90.000000

# Invalid longitude
> GEOADD places 181 0.3 test2
(error) ERR invalid longitude,latitude pair 181.000000,0.300000
```

In this stage, you'll implement validating latitude and longitude values and returning error messages as shown above. The tester is lenient with error message formats, so you don't have to use the exact error message format mentioned above.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send multiple `GEOADD` commands.

If the coordinates are invalid, it will expect an error response. For example:

```bash
# Expecting error
$ redis-cli GEOADD location_key 200 100 foo
(error) ERR invalid longitude,latitude pair 200,100
```

The returned value must:

- Be a [RESP simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors) i.e. start with `-` and end with `\r\n`
- The error message must start with `ERR`, like standard Redis error messages
- The error message must contain the word "latitude" if the latitude is invalid
- The error message must contain the word "longitude" if the longitude is invalid

For example, if the latitude is invalid, valid error messages that the tester will accept are:

```bash
-ERR invalid latitude argument\r\n
-ERR invalid latitude value\r\n
-ERR latitude value (200.0) is invalid\r\n
```

### Notes

- You don't need to implement storing locations yet, we'll get to that in later stages.
- The boundary of latitude are clipped at -/+85.05112878° instead of -/+90°. This is because of the [Web Mercator projection](https://en.wikipedia.org/wiki/Web_Mercator_projection) that Redis uses.
