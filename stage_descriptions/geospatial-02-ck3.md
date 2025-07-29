In this stage, you'll add support for validating the latitude and longitude provided in the `GEOADD` command.

### The `GEOADD` command (Validate coordinates)
Latitudes and longitudes used in the `GEOADD` command should be in a certain range as per [EPSG:3857](https://epsg.io/3857). Valid longitudes are from -180 to 180 degrees. Valid latitudes are from -85.05112878 to 85.05112878 degrees. Both of these limits are inclusive.
Example use case:

```
# Invalid latitude
> GEOADD places 180 90 test1
(error) ERR invalid longitude,latitude pair 180.000000,90.000000

# Invalid longitude
> GEOADD places 181 0.3 test2
(error) ERR invalid longitude,latitude pair 181.000000,0.300000
```

### Tests
The tester will execute your program like this:
```
./your_program.sh
```
It will then send multiple `GEOADD` commands specifying one or more location to add. For valid coordinates, the tester will expect the response to be the usual response of the `GEOADD` command. For invalid coordinates values, it will expect an error.

For example, the tester might send your program a command like this:

```
$ redis-cli GEOADD location_key 200 100 foo

# Expecting error
(error) ERR invalid longitude,latitude pair 200,100
```

The value is RESP simple error, which is RESP-encoded as

```
-ERR invalid longitude,latitude pair 200,100\r\n
```

### Notes

- In case of out-of-bounds values for latitude or longitude, the tester is lenient but structured in checking error messages.
    - The error response does not need to match Redis’s exact error message format.
    - The error response must:
        - Start with the phrase "ERR invalid" (case-insensitive)
         - Contain the word latitude if the latitude value is invalid
        - Contain the word longitude if the longitude value is invalid
        - Contain both "latitude" and "longitude" if both the latitudes and longitudes are invalid.

    - Examples that will pass:
        - ERR Invalid latitude value (In case of invalid latitude and valid longitude)
        - ERR invalid longitude: must be between -180 and 180 (In case of invalid longitude but valid latitude value)
        - ERR invalid longitude,latitude pair (In case of one or both invalid values)
        - ERR invalid longitude and latitude range (In case of one or both invalid values)

    - Examples that will not pass:
        - ERR invalid (In case of one or both invalid values)
        - ERR invalid longitude (In case of invalid latitude but valid longitude)
        - ERR invalid latitude (In case of invalid longitude but valid latitude)
