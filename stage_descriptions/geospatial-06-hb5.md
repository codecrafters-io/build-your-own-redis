Your `GEOPOS` returns placeholder coordinates for existing members. In this
stage, you'll decode each member's stored score so the response reports its
geographic position.

### Decoding latitude and longitude from score

The score combines latitude and longitude grid indexes. Separating those indexes
identifies the cell containing the location. Return the center of that cell.
Encoding discarded some precision, so decoding can't reproduce every original
coordinate exactly.

Use the decoding specification in the
[geocoding reference](https://github.com/codecrafters-io/redis-geocoding-algorithm).
It includes:

- A description of the algorithm used, along with pseudocode
- Code samples in multiple languages.
- A set of locations & scores to test against

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will add multiple locations using `ZADD`, bypassing `GEOADD`. Decode the
scores themselves rather than relying on previously saved input coordinates.

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

- The tester accepts an absolute difference of up to `0.000001` degrees from
  each decoded cell-center coordinate. For the example above, these values are
  accepted:
  - `["2.2944715", "48.8584625"]`
  - `["2.294472", "48.858463"]`
  - `["2.294471594", "48.858462987"]`
