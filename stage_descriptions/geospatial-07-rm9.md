In this stage, you'll add support for searching locations near a coordinate within a given radius.

### The GEOSEARCH command

The `GEOSEARCH` command lets you search for locations near a given coordinate within a specified area.

It supports several search modes. In our implementation, we'll focus only on the `FROMLONLAT` mode with distance unit in meters. The `FROMLONLAT` mode searching by directly specifying longitude and latitude. The syntax for this 

```
GEOSEARCH <key> FROMLONLAT <longitude> <latitude> BYRADIUS <distance> m
```

For example, to search for locations within 100,000 meters of the point (longitude: 15, latitude: 37) stored in the places key, you can use:

```bash
> GEOSEARCH places FROMLONLAT 15 37 BYRADIUS 100000 m
2) "Catania"
```

It returns a RESP Array of member names, where each member's name is a encoded as a bulk string.

### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will add multiple locations using the `GEOADD` command.

```bash
$ redis-cli
> GEOADD places 13.361389 38.115556 Palermo
> GEOADD places 15.087269 37.502669 Catania
```

The tester will then send multiple `GEOSEARCH` commands specifying a latitude and longitude pair with `BYRADIUS` option specifying the distance and unit. For example, it may send the following command.

```bash
> GEOSEARCH places FROMLONLAT 15 37 BYRADIUS 100000 m

# Expecting ["Catania"]
```

The value is a RESP array, which is encoded as:

```bash
*1\r\n
$7\r\n
Catania\r\n
```

### Notes
- The tester will only test using the `meters` unit.