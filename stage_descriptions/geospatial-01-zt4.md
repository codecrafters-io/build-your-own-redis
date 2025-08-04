In this stage, you'll add support for responding to the `GEOADD` command.

### The `GEOADD` command
The `GEOADD` command adds a location (with longitude, latitude, and name) to a key. It stores the location as a [sorted set](https://redis.io/docs/latest/develop/data-types/sorted-sets/) under the specified key. If the key doesn’t exist, a new sorted set is created under the specified name and the location is inserted to it. If the key exists, the location is inserted in the sorted set.

The syntax for `GEOADD` command is

```
GEOADD <key> <longitude> <latitude> <name>
```

Example usage:

```bash
> GEOADD places 13.361389 38.115556 "Palermo"
(integer) 1
```

It returns the count of elements added, encoded as a RESP Integer.

### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `GEOADD` command specifying a key, laitude, longitude, and location name.

```bash
$ redis-cli GEOADD places 15.09 37.50 Catania
```

### Notes
- In this stage, you will only implement responding the the `GEOADD` command. You don't need to store the locations yet. We'll get to storing the locations in the later stages.