In this stage, you'll add support for adding a single location under a key using the `GEOADD` command.

### The `GEOADD` command
The `GEOADD` command adds a location (with longitude, latitude, and name) to a key. If the key doesn’t exist, it is created. If the key exists, the location is appended to that key.
The syntax for `GEOADD` command is

```
GEOADD <key> <longitude> <latitude> <name>
```

Example usage:

```
> GEOADD places 13.361389 38.115556 "Palermo"
(integer) 1
```

It returns the count of elements added, encoded as a RESP Integer.

### Tests
The tester will execute your program like this:

```
./your_program.sh
```

It will then send a `GEOADD` command specifying a key, random values of latitude and longitude, and also a random name for that coordinate.

```
$ redis-cli GEOADD places 15.09 37.50 Catania
```

The tester will verify that the response to the command is `:1\r\n`, which is 1 (the number of locations added), encoded as a RESP Integer.

### Notes
- You will only implement adding one location per `GEOADD` command. 
