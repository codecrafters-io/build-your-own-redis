In this stage, you'll add support for retrieving a bit from a bitmap using the `GETBIT` command.

### The `GETBIT` Command

The `[GETBIT](https://redis.io/docs/latest/commands/getbit/)` command returns the bit stored at a given `offset` in a bitmap.

If the key does not exist, Redis treats the bitmap as all zeros and returns `0` for any offset.

Example usage:

```bash
> SETBIT bit_key 2 1
(integer) 0

> GETBIT bit_key 2
(integer) 1
> GETBIT bit_key 3
(integer) 0
> GETBIT new_key 2
(integer) 0
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SETBIT` command to create a bitmap.

```bash
$ redis-cli SETBIT bit_key 2 1
```

The tester will then send a `GETBIT` command for a bit that was set.

```bash
$ redis-cli GETBIT bit_key 2
```

The tester will expect the response to be `:1\r\n`, which is 1 encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

It will also send a `GETBIT` command for an unset offset on the same key.

```bash
$ redis-cli GETBIT bit_key 3
```

The tester will expect the response to be `:0\r\n`.

The tester will also send a `GETBIT` command for a key that does not exist.

```bash
$ redis-cli GETBIT new_key 2
```

In this case, your program should also respond with `:0\r\n`.

### Notes

- In this stage, you only need to handle `GETBIT` for a bit you just set with `SETBIT`, an unset offset on that key, and a missing key. Offsets beyond the current string length also return `0` 

