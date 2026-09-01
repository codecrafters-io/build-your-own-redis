In this stage, you'll add support for creating a [bitmap](https://redis.io/docs/latest/develop/data-types/bitmaps/) using the `SETBIT` command.

### Redis Bitmaps

Bitmaps let you store individual bits (`0` or `1`). Unlike lists or sorted sets, they are not a separate Redis data type. They are strings that you can access and update at the bit level.

Because each value takes only one bit, bitmaps are extremely space-efficient. This makes them useful for use cases like tracking subscriptions, counting daily active users, or any scenario where you need a yes/no flag for a large population.

For example, if you were using a bitmap to track newsletter subscriptions, the contents might look like this:

```
newsletter_subscribers: 10001010
```

Here, user numbers `0`, `4`, and `6` have subscribed. Redis numbers bits from left to right, starting at offset `0`.

### The `SETBIT` Command

The [SETBIT](https://redis.io/docs/latest/commands/setbit/) command is used to set a bit to `0` or `1` in the string value stored at `key`.

When `key` does not exist, a new string is created and the bit is set.

Example usage:

```bash
> SETBIT bitmap_key 3 1
(integer) 0
```

The `SETBIT` command takes the key, an offset, and a value (`0` or `1`) as arguments. It returns the original bit at that offset. On a new key, that is always `0`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SETBIT` command specifying a key, offset, and value.

```bash
$ redis-cli SETBIT bitmap_key 3 1
```

The tester will verify that the response to the command is `:0\r\n`, which is 0 (the original bit at that offset), encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers). 

### Notes

- In this stage, you'll only need to handle `SETBIT` on a new key with a single bit. We'll get to updating existing bitmaps in later stages.
- Store the bitmap as a string (a byte array). Redis does not have a separate bitmap type. `SETBIT` command creates or grows a string. See `[lookupStringForBitCommand](https://github.com/redis/redis/blob/unstable/src/bitops.c)` in the official implementation.
- Redis treats offset `0` as the most significant bit of the first byte. For example, `SETBIT key 1 1` stores the byte `01000000`. This will matter when you implement `GETBIT` and `BITCOUNT` in later stages.

