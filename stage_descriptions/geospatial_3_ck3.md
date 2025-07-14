In this stage, you'll add support for validating the latitude and longitude provided in the `GEOADD` command.

### The `GEOADD` command (Validating Coordinates)
The latitude and longitudes used in the GEOADD command should be in a certain range. Valid longitudes are from -180 to 180 degrees. Valid latitudes are from -85.05112878 to 85.05112878 degrees. Both of these limits are inclusive.
Example use case:

```
# Invalid latitude
> GEOADD places 180 90 test
(error) ERR invalid longitude,latitude pair 180.000000,90.000000

# Invalid longitude
> geoadd places 181 0.3 test
(error) ERR invalid longitude,latitude pair 181.000000,0.300000
```

In the response, both latitude and longitude values are  rounded to 6 digits after the decimal point.

### Tests
The tester will execute your program like this:
```
./your_program.sh
```
It will then send multiple GEOADD commands specifying one or more location to add. For valid co-ordinates, the tester will expect the response to be the usual response of the GEOADD command. For invalid co-ordinate values, it will expect an error.


For example, the tester will expect the response of the following command

```
$ redis-cli
> GEOADD location_key 200 100 foo
```

to be
```
(error) ERR invalid longitude,latitude pair 180.000000,90.000000
```

which is RESP-encoded as

```
-ERR invalid longitude,latitude pair 180.000000,90.000000\r\n
```

### Notes
If a GEOADD command specifies multiple locations, and if the co-ordinates of any one of those locations is invalid, it returns the error message and all the locations are discarded.