In this stage, you'll add support for growing a bitmap with `SETBIT`, and for checking its length with `STRLEN`.

### Growing a bitmap

When `SETBIT` uses an offset past the end of the string, Redis grows the string so that the offset fits. The string is padded with zeros, and the target bit is set.

[STRLEN](https://redis.io/docs/latest/commands/strlen/) returns the length of that string in bytes, not bits. Offsets `0` through `7` fit in one byte. Offset `8` is the first bit of a second byte. After `SETBIT` grows a key, `STRLEN` is `floor(offset / 8) + 1`. For offset `10`, that is `2`.

```bash
> SETBIT bitmap_key 1 1
(integer) 0
```

After the `SETBIT` command runs, Redis creates a one-byte string `01000000`.

```bash
> SETBIT bitmap_key 10 1
(integer) 0
```

After the second `SETBIT` command runs, Redis appends a second byte and then sets offset `10` to `1`. The final string becomes `01000000 00100000`.

```bash
> STRLEN bitmap_key
(integer) 2
```



### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SETBIT` command to create a one-byte bitmap.

```bash
$ redis-cli SETBIT bitmap_key 1 1
```

The tester will send a `STRLEN` command for that key.

```bash
$ redis-cli STRLEN bitmap_key
```

The tester will expect the response to be `:1\r\n`, which is 1 encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

It will then send a `SETBIT` command with an offset in a second byte.

```bash
$ redis-cli SETBIT bitmap_key 10 1
```

The tester will expect the response to be `:0\r\n`.

The tester will then send `GETBIT` commands for the new bit

```bash
$ redis-cli GETBIT bitmap_key 10
```

The tester will expect the response to be `:1\r\n`.

```bash
$ redis-cli STRLEN bitmap_key
```

The tester will expect the response to be `:2\r\n`.

### Notes

- In this stage, you'll only need to handle creating or growing a key by a small offset (creating the key if it does not exist). You won't need to handle out-of-range offsets.
- `STRLEN` returns the string's length in bytes. If the key does not exist, `STRLEN` returns `0`.

