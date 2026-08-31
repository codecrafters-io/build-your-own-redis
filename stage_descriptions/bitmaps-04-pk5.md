In this stage, you'll add support for using `GET` on a key created with `SETBIT`.

### Strings and bitmaps

As bitmaps are strings, string commands work on keys created with `SETBIT`. `SETBIT` writes bits into the string; `GET` returns that string.

Offset `0` is the most significant bit of the first byte. Setting bit `1` produces the byte `01000000`, which is the ASCII character `@`:

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

The tester will expect the response to be `$1\r\n@\r\n`, which is `@` encoded as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- In this stage, you'll only need to handle `GET` on a key created with a single `SETBIT`. Growing the string with a larger offset is covered in the next stage. 
- `GET` returns the string value stored at the key. `SETBIT` writes that value as bytes. Keys created with `SETBIT` and `SET` share that representation.

