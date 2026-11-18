Your server can measure distances between stored locations. In this stage,
you'll add `GEOSEARCH` to return the names of locations within a radius of a
supplied longitude and latitude.

### The `GEOSEARCH` command

The `GEOSEARCH` command lets you search for locations within a given radius.

It supports several search modes. In our implementation, we'll focus only on the
`FROMLONLAT` mode. The `FROMLONLAT` mode searches by directly specifying
longitude and latitude.

Example usage:

```bash
> GEOSEARCH places FROMLONLAT 2 48 BYRADIUS 100000 m
1) "Paris"
```

With the Paris location from earlier stages stored in `places`, this command
finds it within 100,000 meters of longitude `2`, latitude `48`.

The response is a RESP array containing the names of the locations that match
the search criteria, each encoded as a
[RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

Note that there are two options we passed to the command:

- `FROMLONLAT <longitude> <latitude>` specifies the center point for the search.
- `BYRADIUS <radius> <unit>` specifies the circular search area. The tests use
  `m` for meters.

Redis supports other such options, but in this challenge we'll only use
`FROMLONLAT` and `BYRADIUS`.

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

The tester will then send multiple `GEOSEARCH` commands:

```bash
> GEOSEARCH places FROMLONLAT 2 48 BYRADIUS 100000 m
# Expecting ["Paris"]

> GEOSEARCH places FROMLONLAT 2 48 BYRADIUS 500000 m
# Expecting ["Paris", "London"] (Any order)

> GEOSEARCH places FROMLONLAT 11 50 BYRADIUS 300000 m
# Expecting ["Munich"]
```

The tester will validate that the response is a RESP array, for example

```
*2\r\n
$5\r\n
Paris\r\n
$6\r\n
London\r\n
```

Locations can be returned in any order.

### Notes

- The tester will always use the `FROMLONLAT` and `BYRADIUS` options when
  sending a `GEOSEARCH` command.
