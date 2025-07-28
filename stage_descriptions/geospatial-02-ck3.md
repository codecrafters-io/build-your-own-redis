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

The tester will also send your program a `GEOADD` command specifying a non-numeric value in the latitude/longitude field. In this case, it will expect an error. For example, in case of the following command, it will expect an error.

```
$ redis-cli GEOADD location_key foo bar location_name

# Expecting RESP simple error
(error) ERR value is not a valid float
```

### Notes
- In case of out-of-bounds values of latitude or longitude, the tester is lenient in checking error messages so you don't have to stick to the exact format Redis uses. The exact format it checks for is `ERR invalid` (case-insensitive). Examples of error message strings that will pass the tests:
    - `ERR invalid longitude,latitude pair 200,100`
    - `ERR Invalid longitude,latitude`
    - `ERR invalid`
