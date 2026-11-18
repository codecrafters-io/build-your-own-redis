Your `GEOADD` validates coordinates. In this stage, you'll store the location's
name as a sorted-set member with placeholder score `0`, making it visible
through `ZRANGE`.

### Storing locations in a sorted set

The locations added using the `GEOADD` command are stored in a sorted set. Redis
internally calculates a score for the specified location using a location's
latitude and longitude.

For example, the following two commands are equivalent in Redis.

```bash
# Adding a location
$ redis-cli GEOADD places_key 2.2944692 48.8584625 location

# This command is equivalent to the command above
$ redis-cli ZADD places_key 3663832614298053 location
```

The example shows Redis's calculated score. Your current task only needs the
placeholder score.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `GEOADD` command:

```bash
$ redis-cli GEOADD places 2.2944692 48.8584625 Paris
# Expect: (integer) 1
```

The tester will then send a `ZRANGE` command to validate that the location was
added to the sorted set:

```bash
$ redis-cli ZRANGE places 0 -1
# Expect RESP Array: ["Paris"]
```

### Notes

- The implementation of the `ZRANGE` command is covered in the sorted sets
  extension.
