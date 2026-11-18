This extension adds
[bitmap](https://redis.io/docs/latest/develop/data-types/bitmaps/) operations
for storing and combining yes/no flags. In this stage, you'll implement `SETBIT`
to change one bit in a string and return the bit's previous value.

### Redis bitmaps

Redis bitmaps are string values viewed as individual bits (`0` or `1`). Each
byte holds eight bits. Offset `0` identifies the most significant bit of the
first byte, the bit with value `128`. Offsets increase toward the least
significant bit, then continue in the next byte.

Bitmaps can track subscriptions or daily activity with one bit per user.

For example, if you were using a bitmap to track newsletter subscriptions, the
contents might look like this:

```
newsletter_subscribers: 10001010
```

Here, user numbers `0`, `4`, and `6` have subscribed. Redis numbers bits from
left to right, starting at offset `0`.

### The `SETBIT` command

The [SETBIT](https://redis.io/docs/latest/commands/setbit/) command is used to
set a bit to `0` or `1` in the string value stored at `key`.

When `key` does not exist, a new string is created and the bit is set.

Example usage:

```bash
> SETBIT bitmap_key 3 1
(integer) 0
```

The arguments are the key, an offset, and a value (`0` or `1`). Return the
original bit at that offset as a RESP integer, encoded as `:<number>\r\n`. On a
new key, the original bit is `0`. Setting an existing `1` bit to `0` returns
`1`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SETBIT` command specifying a key, offset, and value.

```bash
$ redis-cli SETBIT bitmap_key 3 1
```

The tester will verify that the response to the command is `:0\r\n`, which is 0
(the original bit at that offset), encoded as a
[RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

### Notes

- Bitmap and string commands operate on the same stored bytes. Later stages make
  that shared representation visible through `SET` and `GET`.
- Redis treats offset `0` as the most significant bit of the first byte. For
  example, `SETBIT key 1 1` stores the byte `01000000`. This will matter when
  you implement `GETBIT` and `BITCOUNT` in later stages.
