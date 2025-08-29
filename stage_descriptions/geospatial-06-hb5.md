In this stage, you'll add support for decoding the coordinates of a location.

### Decoding latitude and longitude from score

The algorithm to get back the latitude and longitude is essentially the reverse of the one used to compute the score from them.

The [GitHub repository](https://github.com/codecrafters-io/redis-geocoding-algorithm) we referenced earlier explains how this conversion is done. It includes:

- A description of the algorithm used, along with pseudocode
- Code samples in multiple languages.
- A set of locations & scores to test against

Here's the [repository link](https://github.com/codecrafters-io/redis-geocoding-algorithm).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will add multiple locations using the `ZADD` command. The scores used are valid scores that can be converted back to latitude and longitude values.

```bash
$ redis-cli
> ZADD location_key 3663832614298053 "Foo"
> ZADD location_key 3876464048901851 "Bar"
> ZADD location_key 3468915414364476 "Baz"
> ZADD location_key 3781709020344510 "Caz"
```

The tester will then send multiple `GEOPOS` commands:

```bash
> GEOPOS location_key Foo
# Expecting [["2.294471561908722", "48.85846255040141"]]
```

The tester will validate that the response is a RESP array, which is encoded as:

```
*1\r\n
*2\r\n
$17\r\n
2.294471561908722\r\n
$17\r\n
48.85846255040141\r\n
```

### Notes

- The conversion from latitude/longitude to score and back is lossy, so the tester will be lenient in checking the coordinates provided - it will accept any coordinates that are within 6 decimal places of the original values. For example, for the example shown above, any of the following values will be accepted:
  - `["2.2944715", "48.8584625"]`
  - `["2.294472", "48.858463"]`
  - `["2.294471594", "48.858462987"]`
