In this stage, you'll add support for responding to the `GEOADD` command.

### The `GEOADD` command
The `GEOADD` command adds a location (with longitude, latitude, and name) to a key. It stores the location as a [sorted set](https://redis.io/docs/latest/develop/data-types/sorted-sets/) under the specified key. If the key doesn’t exist, a new sorted set is created under the specified name and the location is inserted to it. If the key exists, the location is inserted in the sorted set.

Example usage:

```bash
> GEOADD places -0.0884948 51.506479 "London"
(integer) 1
```
The argument order of `GEOADD` argument – key, longitude, latitude, member (longitude first, then latitude).

It returns the count of elements added, encoded as a RESP Integer.

### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `GEOADD` command specifying a key, latitude, longitude, and location name.

```bash
$ redis-cli GEOADD places 11.5030378 48.164271 Munich
```

The tester will expect the response to be `:1\r\n`, which is 1(number of locations added) encoded as a RESP integer.

### Notes
- In this stage, you will only implement responding the the `GEOADD` command. You don't need to store the locations yet. We'll get to storing the locations in the later stages.