In this stage, you'll add support for `BITOP AND` when the source bitmaps have different lengths.

### `BITOP AND` with different lengths

When the source strings have different lengths, Redis treats the shorter ones as if they were padded with zeros up to the length of the longest string. The destination is as long as that longest string.

```bash
> SETBIT key1 1 1
(integer) 0
> SETBIT key1 10 1
(integer) 0
> SETBIT key2 1 1
(integer) 0
```

In this example, we have two bitmaps: `key1` is `01000000 00100000` (2 bytes) and `key2` is `01000000` (1 byte), created using `SETBIT` commands.

```bash
> BITOP AND dest key1 key2
(integer) 2
> GETBIT dest 1
(integer) 1
> GETBIT dest 10
(integer) 0
```

During the `BITOP AND` operation, Redis treats the shorter string as zero-padded. In this example, `key2` is treated as `01000000 00000000` (second byte is padded with `0`s). 

Only bit `1` is set in both keys. So `dest` will hold the value `01000000 00000000`. `BITOP` returns `2`, the length of the destination.

Additional notes:

- A missing key is treated as a string of zeros of the same length as the longest source.



### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send `SETBIT` commands to create two bitmaps of different lengths.

```bash
$ redis-cli SETBIT key1 1 1
$ redis-cli SETBIT key1 10 1
$ redis-cli SETBIT key2 1 1
```

The tester will send a `BITOP AND` command that stores the result at a new key.

```bash
$ redis-cli BITOP AND dest key1 key2
```

The tester will expect the response to be `:2\r\n`, which is 2 encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

It will then send `GETBIT` commands for the destination key.

```bash
$ redis-cli GETBIT dest 1
```

The tester will expect the response to be `:1\r\n`.

```bash
$ redis-cli GETBIT dest 10
```

The tester will expect the response to be `:0\r\n`.

### Notes

- `BITOP` also supports other [operations](https://redis.io/docs/latest/commands/bitop/) like `OR`, `XOR`, and `NOT`. The `BITOP OR` operation is covered in later stages. We won't deal with `XOR` and `NOT` in this challenge.

