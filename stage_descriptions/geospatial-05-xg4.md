In this stage, you'll add support for responding to the `GEOPOS` command.

### The `GEOPOS` command

The `GEOPOS` command returns the longitude and latitude of the specified location. Its syntax is

```
GEOPOS <key> <location1>
```

Example usage:
```bash
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


Since the latitude and longitude values are calculated from the score of each location, an extra step is required. You will implement this in the next stage. For now, you only need to respond with a valid floating point numbers for latitude and longitude. For example, you may hardcode every latitude and longitude to be 0.

### Tests
The tester will execute your program like this:
```bash
$ ./your_program.sh
```

It will add multiple locations using the `ZADD` command. It will use a score which will be equivalent to a latitude and longitude.

```bash
$ redis-cli
> ZADD location_key 3477108430792699 "Foo"
> ZADD location_key 3876464048901851 "Bar"
> ZADD location_key 3468915414364476 "Baz"
> ZADD location_key 3781709020344510 "Caz"
```

The tester will then send multiple `GEOPOS` commands, each specifying a single location that may or may not have been added. For example, the tester might send your program a command like this:

```bash
> GEOPOS location_key Foo
# Expecting [["0", "0"]]
```

It will expect the value to be a RESP array, which contains another RESP array. The elements of the inner array should be the two bulk strings. Each bulk string should be a valid floating point number when parsed.

The tester will also send a `GEOPOS` command using a non-existent key. 

```bash
> GEOPOS non_existent_key Foo
# Expecting: *0\r\n
```

It will expect the response to be a RESP nil array, which is encoded as `*0\r\n`.

### Notes

- In this stage, you will only implement responding to the `GEOPOS` command using valid floating point numbers. We'll get to responding with actual values of latitude and longitude in the next stage.
