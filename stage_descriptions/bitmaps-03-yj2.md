In this stage, you'll add support for using `GETBIT` on a key created with `SET`.

### Strings and bitmaps

As bitmaps are strings, bitmap commands work on any string value, including keys created with `SET`.

These two views operate on the same bytes. `SET` writes the string. `GETBIT` reads one bit of it.

For example, the ASCII character `A` is the byte `01000001`. Offset `0` is the most significant bit of that byte:

```bash
> SET mykey "A"
OK
> GETBIT mykey 1
(integer) 1
> GETBIT mykey 7
(integer) 1
> GETBIT mykey 0
(integer) 0
```

Here, bits `1` and `7` are set and bit `0` is not.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SET` command to store a one-byte string. For example:

```bash
$ redis-cli SET mykey "A"
```

The tester will then send `GETBIT` commands for offsets in that string.

```bash
$ redis-cli GETBIT mykey 1
```

The tester will expect the response to be `:1\r\n`, which is 1 encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

```bash
$ redis-cli GETBIT mykey 7
```

The tester will expect the response to be `:1\r\n`.

```bash
$ redis-cli GETBIT mykey 0
```

The tester will expect the response to be `:0\r\n`.

### Notes

- In this stage, you'll only need to handle `GETBIT` on a key created with `SET`. Reading a bitmap back with `GET` is covered in the later stages.
- Redis applies `GETBIT` to the string's bytes. Keys created with `SET` and `SETBIT` share that representation. See [the official implementation](https://github.com/redis/redis/blob/e1d7d50f9c244ce52f724b279fcb19773fffa98c/src/bitops.c#L915-L917).

