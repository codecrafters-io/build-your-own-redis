This extension lets clients store named locations and search for nearby ones.
Validation and storage come later. In this stage, you'll recognize `GEOADD`, the
command that adds a location, and return the placeholder integer `1`.

### Extension prerequisites

This stage depends on the
[**Sorted Sets**](https://redis.io/docs/latest/data-types/sorted-sets/)
extension. Before attempting this extension, please make sure you've completed
the Sorted Sets extension.

### The `GEOADD` command

The [`GEOADD` command](https://redis.io/docs/latest/commands/geoadd/) adds a
named location to a key. Longitude measures east/west position and latitude
measures north/south position, both in degrees.

Example usage:

```bash
> GEOADD places -0.0884948 51.506479 "London"
(integer) 1
```

The arguments `GEOADD` accepts are:

1. `key`: The key to store the location in.
2. `longitude`: The longitude of the location.
3. `latitude`: The latitude of the location.
4. `member`: The name of the location.

It returns the count of elements added, encoded as a RESP Integer.

For now, return `:1\r\n` for the single-location form shown here.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `GEOADD` command:

```bash
$ redis-cli GEOADD places 11.5030378 48.164271 Munich
```

The tester will expect the response to be `:1\r\n`, which is 1 (number of
locations added) encoded as a RESP integer.
