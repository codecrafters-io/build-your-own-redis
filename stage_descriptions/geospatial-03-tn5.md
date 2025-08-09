In this stage, you'll add support for storing locations in a sorted set.

### Storing locations in sorted set

The locations added using the `GEOADD` command are stored in a sorted set. Redis internally calculates a score for the specified location using a location's latitude and longitude.

For example, the following two commands are equivalent in Redis.

```bash
# Adding a location
$ redis-cli GEOADD places_key 2.2944692 48.8584625 location

# This command is equivalent to the command above
$ redis-cli ZADD places_key 3663832614298053 location
```

where score is calculated using the location's latitude and longitude values using an algorithm. We'll get to implementing this algorithm in the later stage.

For now, you can hardcode a location's score to be 0 for all the locations.

### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `GEOADD` command specifying a key, laitude, longitude, and location name.

```bash
$ redis-cli GEOADD places 2.2944692 48.8584625 Paris
# Expect: (integer) 1
```

The tester will then send a `ZRANGE` command to your program specifying the key used in `GEOADD` command.
```bash
$ redis-cli ZRANGE places 0 -1
# Expect RESP Array: ["Paris"]
```

### Notes
- In this stage, you can hardcode the score of the location to be 0. We'll get to calculating the value of score using latitude and longitude in the next stage.