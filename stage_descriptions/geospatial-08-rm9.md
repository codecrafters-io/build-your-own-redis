In this stage, you'll add support for searching locations near a coordinate within a given radius using the `GEOSEARCH` command.

### The GEOSEARCH command

The `GEOSEARCH` command lets you search for locations near a given coordinate within a specified area.

It supports several search modes. In our implementation, we'll focus only on the `FROMLONLAT` mode with distance unit in meters. The `FROMLONLAT` mode searching by directly specifying longitude and latitude. 

For example, to search for locations within 100000 meters of the point (longitude: 2, latitude: 48) stored in the `places` key, you can use:

```bash
> GEOSEARCH places FROMLONLAT 2 48 BYRADIUS 100000 m
1) "Paris"
```

- `FROMLONLAT <longitude> <latitude>` — This option specifies the center point for the search.
- `BYRADIUS <radius> <unit>` — This option searches within a circular area of the given radius and unit (m, km, mi, etc.).

Redis supports other values for search origin option and shape option, but here we'll only use `FROMLONLAT` and `BYRADIUS`.

It returns a RESP Array of member names, where each member's name is a encoded as a bulk string.

### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will add multiple locations using the `GEOADD` command.

```bash
$ redis-cli
> GEOADD places 11.5030378 48.164271 "Munich"
> GEOADD places 2.2944692 48.8584625 "Paris"
> GEOADD places -0.0884948 51.506479 "London"
```

The tester will then send multiple `GEOSEARCH` commands specifying a latitude and longitude pair with `BYRADIUS` option specifying the distance and unit. For example, it may send the following command.

```bash
> GEOSEARCH places FROMLONLAT 2 48 BYRADIUS 100000 m
# Expecting ["Paris"]

> GEOSEARCH places FROMLONLAT 2 48 BYRADIUS 500000 m
# Expecting ["Paris, "London"] (Any order)

> GEOSEARCH places FROMLONLAT 11 50 BYRADIUS 300000 m
# Expecting ["Munich"]
```

The value is a RESP array, which is encoded as:

```
*2\r\n
$5\r\n
Paris\r\n
$6\r\n
London\r\n
```

### Notes
- The tester will only test using the `meters` unit.
- The locations returned can be in any order since we are not implementing `ASC` or `DESC` option.