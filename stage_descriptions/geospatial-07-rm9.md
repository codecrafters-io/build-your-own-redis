In this stage, you'll add support for searching locations near a coordinate within a given radius.

### The GEOSEARCH command

The `GEOSEARCH` command lets you search for locations near a given coordinate within a specified area.

It supports several search modes, but in our implementation, we'll focus only on the `FROMLONLAT` option, which allows searching by directly specifying longitude and latitude. The syntax for this is:

```
GEOSEARCH key FROMLONLAT longitude latitude BYRADIUS distance [km|ft|m|mi]
```

For example, to search for locations within 200 kilometers of the point (longitude: 15, latitude: 37) stored in the places key, you can use:

```
> GEOSEARCH places FROMLONLAT 15 37 BYRADIUS 200 km
1) "Palermo"
2) "Catania"
```

It returns a RESP Array of member names, where each member's name is a encoded as a bulk string.

### Tests
The tester will execute your program like this:

```
./your_program.sh
```

It will add multiple locations using the `GEOADD` command.

```
$ redis-cli
> GEOADD places 13.361389 38.115556 Palermo
> GEOADD places 15.087269 37.502669 Catania
```

The tester will then send multiple `GEOSEARCH` commands specifying a latitude and longitude pair with `BYRADIUS` option specifying the distance and unit. For example, it may send the following command.

```
> GEOSEARCH places FROMLONLAT 15 37 BYRADIUS 200 km

# Expecting ["Palermo", "Catania"]
```

The value is a RESP array, which is encoded as:

```
*2\r\n
$7\r\n
Palermo\r\n
$7\r\n
Catania\r\n
```

### Notes
In this stage, you will only implement the `BYRADIUS` option. We'll get to implementing the BYBOX option in the next stage.