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

It will add multiple locations using the `GEOADD` command.

```
$ redis-cli
> GEOADD location_key 19.0872 33.5026 "Foo"
> GEOADD location_key 49.125 72.991 "Bar"
> GEOADD location_key 10.0872 34.5026 "Baz"
> GEOADD location_key 41.125 73.991 "Caz"
```

The tester will then send multiple `GEOPOS` commands, each specifying a single location that may or may not have been added. For example, the tester might send your program a command like this:

```
> GEOPOS places Foo

# Expecting [["19.0872", "33.5026"]]
```

The value is a RESP array, which is encoded as
```
*1\r\n
*2\r\n
$7\r\n
19.0872\r\n
$7\r\n
33.5026\r\n
```

### Notes

- The tester will be lenient in checking the coordinates provided. The latitude and longitude returned by the server should match the values provided in the `GEOADD` command with a precision of up to 4 decimal places when rounded off.

  * For example, if a location was added using `GEOADD key 20.123456 30.123001`, any of the following will be accepted:

    * `20.1235 30.1230`
    * `20.123456 30.123000`
    * `20.123490 30.123045`
    * `20.123459 30.123001`
    * `20.1235001 30.122999`

- If the location key does not exist, you should return an empty array.

- In this stage, you will only implement retrieving a single location using the `GEOPOS` command. We'll get to retrieving multiple locations in a single `GEOPOS` command in the next stage.
