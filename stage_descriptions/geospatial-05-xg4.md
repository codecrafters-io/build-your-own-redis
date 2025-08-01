In this stage, you'll add support for retrieving the coordinates of a location using the `GEOPOS` command.

### The `GEOPOS` command

The `GEOPOS` command returns the longitude and latitude of the specified locations. Its syntax is

```
GEOPOS <key> <location1> <location2> …
```

Example usage:
```
> GEOADD places 15.087269 37.502669 "Catania"
> GEOADD places 12.496365 41.902783 "Rome"

> GEOPOS places Catania Rome non_existent
1) 1) "15.087267458438873"
   2) "37.50266842333162"
2) 1) "12.496366202831268"
   2) "41.90278213378984"
3) (nil)
```

It returns an array with one entry for each location requested.

- If the key does not exist, an empty array is returned `(*0\r\n)`
- If a location exists under the key, its entry is an array of two items:
    - Longitude (Encoded as a Bulk string)
    - Latitude (Encoded as a Bulk string)
- If a location doesn’t exist, the entry is a null bulk string `($-1\r\n)`.

### Tests
The tester will execute your program like this:
```
./your_program.sh
```

It will add multiple locations using the `ZADD` command. It will use a score which will be equivalent to a latitude and longitude.

```
$ redis-cli
> ZADD location_key 3477108430792699 "Foo"
> ZADD location_key 3876464048901851 "Bar"
> ZADD location_key 3468915414364476 "Baz"
> ZADD location_key 3781709020344510 "Caz"
```

The tester will then send multiple `GEOPOS` commands, each specifying a single location that may or may not have been added. For example, the tester might send your program a command like this:

```
> GEOPOS location_key Foo

# Expecting [["19.087197482585907", "33.50259961456723"]]
```

The value is a RESP array, which is encoded as
```
*1\r\n
*2\r\n
$18\r\n
19.087197482585907\r\n
$17\r\n
33.50259961456723\r\n
```

### Notes

- The tester will be lenient in checking the coordinates provided. The latitude and longitude returned by the server should match the values provided in the `GEOADD` command with a precision of up to 4 decimal places when rounded off.

   - For example, for the response of the example shown above, any of the following will be accepted:

      - `19.0872 33.5026`
      - `19.087197 33.502599`
      - `19.08719 33.50260`
      - `19.087199 33.5025996`
      - `19.0872001 33.5026001`

- If the location key does not exist, you should return an empty array.