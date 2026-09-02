In this stage, you'll add support for counting set bits in a bitmap using the `BITCOUNT` command.

### The `BITCOUNT` Command

The [BITCOUNT](https://redis.io/docs/latest/commands/bitcount/) command returns the number of bits set to `1` in a bitmap.

With no extra arguments, it counts the whole string. It can also take a `start` and `end` index. These indexes are **bytes**, not bits. Both are inclusive.

Offset `0` through `7` is byte `0`. Offset `8` through `15` is byte `1`. `BITCOUNT key 0 0` counts only the first byte.

```bash
> SETBIT bitmap_key 1 1
(integer) 0
> SETBIT bitmap_key 10 1
(integer) 0
```

After those `SETBIT` commands, the string is `01000000 00100000`. Byte `0` has one set bit. Byte `1` has one set bit.

```bash
> BITCOUNT bitmap_key
(integer) 2
> BITCOUNT bitmap_key 0 1
(integer) 2
```

Calling `BITCOUNT` with no arguments counts the `1`s in every byte. It is the same as calling `BITCOUNT` with `0 1` in this example.

```bash
> BITCOUNT bitmap_key 0 0
(integer) 1
> BITCOUNT bitmap_key 1 1
(integer) 1
```

Calling `BITCOUNT` with `0 0` counts the `1`s in the first byte. Calling it with `1 1` counts the `1`s in the second byte.

Here are some additional notes on how the `BITCOUNT` command behaves with different types of inputs:

- If the key does not exist, `BITCOUNT` returns `0`.
- If `start` is past the end of the string, `BITCOUNT` returns `0`.
- If `end` is past the end of the string, Redis treats it as the last byte.
- If `start` is greater than `end`, `BITCOUNT` returns `0`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send `SETBIT` commands to create a two-byte bitmap.

```bash
$ redis-cli SETBIT bitmap_key 1 1
$ redis-cli SETBIT bitmap_key 10 1
```

After that, the tester will send your program a series of `BITCOUNT` commands with no extra arguments, or with non-negative indexes. For each command, it will expect the number of set bits, encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

As an example, the tester might send your program a command like this:

```bash
$ redis-cli BITCOUNT bitmap_key 0 0
```

The tester will expect the response to be `:1\r\n`.

### Notes

- In this stage, you'll only need to handle `BITCOUNT` with no range, and with non-negative indexes. We won't deal with negative indexes in this challenge.
- `BITCOUNT` supports the `BYTE` and `BIT` [optional arguments](https://redis.io/docs/latest/commands/bitcount/#optional-arguments), but we won't deal with them in this challenge.

