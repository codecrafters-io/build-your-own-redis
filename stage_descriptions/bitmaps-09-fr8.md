In this stage, you'll add support for combining bitmaps with `BITOP OR`.

### The `BITOP OR` Command

`BITOP OR` sets a bit in the destination when that bit is set in at least one source bitmap.

In this example, we have two bitmaps: `key1` is `10001000` and `key2` is `10000010`, created using `SETBIT` commands.

```bash
> BITOP OR dest key1 key2
(integer) 1
> GETBIT dest 0
(integer) 1
> GETBIT dest 4
(integer) 1
> GETBIT dest 6
(integer) 1
```

After performing a `BITOP OR` operation on `key1` and `key2`, we get another bitmap `dest` with the value `10001010` because bits `0`, `4`, and `6` are set in at least one source.

`BITOP` returns the length of the destination string in bytes. Here that is `1`.

When the source strings have different lengths, Redis treats the shorter ones as if they were padded with zeros up to the length of the longest string.

In this example, we have two bitmaps: `key1` is `01000000 00100000` (2 bytes) and `key2` is `01000000` (1 byte).

```bash
> BITOP OR dest key1 key2
(integer) 2
> GETBIT dest 1
(integer) 1
> GETBIT dest 10
(integer) 1
```

`key2` is treated as `01000000 00000000`. Bit `10` is set in `key1` and padded to `0` in `key2`, so it is set in the destination. `dest` holds `01000000 00100000`. `BITOP` returns `2`, the length of the destination.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send `SETBIT` commands to create two bitmaps of the same length.

```bash
$ redis-cli SETBIT key1 0 1
$ redis-cli SETBIT key1 4 1
$ redis-cli SETBIT key2 0 1
$ redis-cli SETBIT key2 6 1
```

The tester will send a `BITOP OR` command that stores the result at a new key.

```bash
$ redis-cli BITOP OR dest key1 key2
```

The tester will expect the response to be `:1\r\n`.

It will then send a `GETBIT` command for a bit that is set in only one source.

```bash
$ redis-cli GETBIT dest 4
```

The tester will expect the response to be `:1\r\n`.

The tester will then send `SETBIT` commands to create two bitmaps of different lengths.

```bash
$ redis-cli SETBIT key3 1 1
$ redis-cli SETBIT key3 10 1
$ redis-cli SETBIT key4 1 1
```

The tester will send a `BITOP OR` command that stores the result at a new key.

```bash
$ redis-cli BITOP OR dest2 key3 key4
```

The tester will expect the response to be `:2\r\n`, which is 2 encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

It will then send a `GETBIT` command for a bit that is past the end of the shorter source.

```bash
$ redis-cli GETBIT dest2 10
```

The tester will expect the response to be `:1\r\n`.

### Notes

- `BITOP` also supports other [operations](https://redis.io/docs/latest/commands/bitop/) like `XOR` and `NOT`, but we won't deal with them in this challenge.

