Your server can read a string as bits. In this stage, you'll expose the other
direction: `GET` must return the bytes built by `SETBIT` as a bulk string.

### Strings and bitmaps

As bitmaps are strings, string commands work on keys created with `SETBIT`.

These two views operate on the same bytes. `SETBIT` writes bits into the string.
`GET` returns that string.

Offset `0` is the most significant bit of the first byte. Setting bit `1`
produces the byte `01000000`, which is the ASCII character `@`:

```bash
> SETBIT mykey 1 1
(integer) 0
> GET mykey
"@"
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SETBIT` command to create a bitmap.

```bash
$ redis-cli SETBIT mykey 1 1
```

The tester will then send a `GET` command for that key.

```bash
$ redis-cli GET mykey
```

The tester will expect the response to be `$1\r\n@\r\n`, which is `@` encoded as
a
[RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- This stage uses bits within one byte, including successive `SETBIT` calls on
  that byte. Growing the string with a larger offset comes later.
