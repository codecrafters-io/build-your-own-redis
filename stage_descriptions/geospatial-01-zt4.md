In this stage, you'll add support for responding to the `GEOADD` command.

### The `GEOADD` command

The [`GEOADD` command](https://redis.io/docs/latest/commands/geoadd/) adds a location (with longitude, latitude, and name) to a key. It stores the location as a [sorted set](https://redis.io/docs/latest/develop/data-types/sorted-sets/) under the specified key.

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

In this stage, you'll only implement the response to the `GEOADD` command. We'll get to validating arguments and storing locations in later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `GEOADD` command:

```bash
$ redis-cli GEOADD places 11.5030378 48.164271 Munich
```

The tester will expect the response to be `:1\r\n`, which is 1 (number of locations added) encoded as a RESP integer.

### Notes

- In this stage, you only need to implement responding to the `GEOADD` command. We'll get to validating arguments and storing locations in later stages.
