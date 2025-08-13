In this stage, you'll add support for calculating the score of a location.

### Location scores

To store locations in a sorted set, Redis converts latitude and longitude values to a single "score".

We've created a [GitHub repository](https://github.com/codecrafters-io/redis-geocoding-algorithm) that explains how this conversion is done. It includes:

- A description of the algorithm used, along with pseudocode
- Python code that implements the algorithm
- A set of locations & scores to test against

Here's the [repository link](https://github.com/codecrafters-io/redis-geocoding-algorithm).

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

The tester will validate that scores are calculated correctly by sending multiple `ZSCORE` commands:

```bash
$ redis-cli ZSCORE places Paris
# Expecting bulk string: "3663832614298053"
```

The calculated scores must match the expected values as described in [this repository](https://github.com/codecrafters-io/redis-geocoding-algorithm).
