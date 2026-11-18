Your server stores location names with score `0`. In this stage, you'll replace
that placeholder with a score encoding the coordinates, so the sorted set
carries each location's position as well as its name.

### Location scores

Redis divides the allowed latitude and longitude ranges into a grid. Each axis
uses a 26-bit cell index. Interleaving those indexes' bits produces a 52-bit
integer score, small enough for a sorted set's 64-bit floating-point score to
represent exactly.

Use the encoding specification in the
[geocoding reference](https://github.com/codecrafters-io/redis-geocoding-algorithm)
for this stage. It includes:

- A description of the algorithm used, along with pseudocode
- Code samples in multiple languages.
- A set of locations & scores to test against

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send multiple `GEOADD` commands:

```bash
$ redis-cli GEOADD places 2.2944692 48.8584625 Paris
# Expect: (integer) 1
$ redis-cli GEOADD places -0.127758 51.507351 London
# Expect: (integer) 1
```

The tester will validate that scores are calculated correctly by sending
multiple `ZSCORE` commands:

```bash
$ redis-cli ZSCORE places Paris
# Expecting bulk string: "3663832614298053"
```

The calculated scores must match the expected values as described in
[the Redis geocoding specification](https://github.com/codecrafters-io/redis-geocoding-algorithm).
