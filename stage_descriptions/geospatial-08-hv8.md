In this stage, you'll add support for searching locations near a coordinate within a rectangular bounding box.

### The GEOSEARCH command (Box Search)
The GEOSEARCH command can also be used to find all points within a box centered at a given pair of latitude and longitude within a rectangle of certain width and height.
The syntax for this is:
```
GEOSEARCH key FROMLONLAT longitude latitude BYBOX width height [km|ft|m|mi]
```

Example usage:

```
> GEOADD cities 13.361389 38.115556 "Palermo"
(integer) 1
> GEOADD cities 15.087269 37.502669 "Catania"
(integer) 1
> GEOADD cities 12.496365 41.902783 "Rome"
(integer) 1

> GEOSEARCH cities FROMLONLAT 15 37 BYBOX 300 300 km
1) "Palermo"
2) "Catania"
```

### Tests
The tester will execute your program like this:
```
./your_program.sh
```

It will add multiple locations with random co-ordinates using the `GEOADD` command.
```
> GEOADD test 0 0 "center"
> GEOADD test 0.5 0 "east"
> GEOADD test -0.5 0 "west"
> GEOADD test 0 0.5 "north"
> GEOADD test 0 -0.5 "south"
> GEOADD test 1 1 "outside"
```

The tester will then send multiple `GEOSEARCH` commands with `FROMLONLAT` and `BYBOX` option specifying random longitude and latitude and bounding box size. For example, it may send the following:


```
> GEOSEARCH test fromlonlat 0 0 bybox 300 300 km
1) "center"
2) "east"
3) "north"
4) "outside"
5) "south"
6) "west"
```

It will  expect the response to be 

```
[
  "center",
  "east",
  "north",
  "outside",
  "south",
  "west",
]
```
which is RESP-Encoded as
```
*6\r\n
$6\r\n
center\r\n
$4\r\n
east\r\n
$5\r\n
north\r\n
$7\r\n
outside\r\n
$5\r\n
south\r\n
$4\r\n
west\r\n
```
