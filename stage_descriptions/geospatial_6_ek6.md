In this stage, you'll add support for calculating the distance between two locations.

### The `GEODIST` command
The `GEODIST` command returns the distance between two members. It can also take a optional last parameter which is the unit in which to express the distance in. The default unit is meters. The valid units are meters(m), kilometers(km), miles(mi), or feet(ft).
The syntax for `GEODIST` command is:
```
GEODIST <key> <location1> <location2> [unit]
```
Example usage:

```
# Distance in meters
> GEODIST places Catania Rome m
"537215.1152"

# Distance in kilometers
> GEODIST places Catania Rome km
"537.2151"

# Distance in feet
> GEODIST places Catania Rome ft
"1762516.7822"

# Distance in miles
> GEODIST places Catania Rome mi
"333.8108"

# Default is meters
 GEODIST places Catania Rome
"537215.1152"
```

It returns the distance as a string, encoded as a RESP Bulk String. The precision of the response is up to 4 digits after the decimal.

Calculating the distance when two latitude and longitude pairs are given is not as straightforward as using Pythagorean theorem. Since the earth is a sphere, we have to account for its curvature as well. The [Haversine's Formula](https://en.wikipedia.org/wiki/Haversine_formula#Example) is used to calculate distance in such cases. See how redis implements it [here](https://github.com/redis/redis/blob/4322cebc1764d433b3fce3b3a108252648bf59e7/src/geohash_helper.c#L228C1-L228C72).


### Tests
The tester will execute your program like this:
```
./your_program.sh
```

It will then add multiple locations using the `GEOADD` command.
```
GEOADD places 15.087269 37.502669 "Catania" 12.496365 41.902783 "Rome"
```

The tester will then send multiple `GEODIST` commands specifying two locations. For example, in this case,

```
GEODIST places Catania Rome
```

it will expect the response to be "166.2742", which is RESP encoded as a bulk string as:

```
$11\r\n
537215.1152\r\n
```

### Notes
- If one or both of the location specified in the `GEODIST` command does not exist, it should return a null bulk string `($-1\r\n)`.
- In this stage, you will only implement returning the distance in meters. We will get to using different units in the next stage.