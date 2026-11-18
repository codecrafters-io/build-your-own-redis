Your server can inspect one bitmap. In this stage, you'll combine two
equal-length bitmaps with `BITOP AND`, keeping only the positions set in both.

### The `BITOP` command

The [BITOP](https://redis.io/docs/latest/commands/bitop/) command runs a bitwise
operation on one or more bitmaps and stores the result at a destination key.

`BITOP AND` sets a bit in the destination only when that bit is set in every
source bitmap. For subscription flags, this identifies users present in both
groups.

In this example, `key1` is `10001000` and `key2` is `10000010`, created using
`SETBIT` commands.

```bash
> BITOP AND dest key1 key2
(integer) 1
> GETBIT dest 0
(integer) 1
> GETBIT dest 4
(integer) 0
> GETBIT dest 6
(integer) 0
```

The destination `dest` contains `10000000`, since only bit `0` is set in both
sources.

`BITOP` returns the length of the destination string in bytes, encoded as a
[RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).
Here the destination string is 1 byte, so the return value is `1`.

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

The tester will send a `BITOP AND` command that stores the result at a new key.

```bash
$ redis-cli BITOP AND dest key1 key2
```

The tester will expect the response to be `:1\r\n`.

It will then send `GETBIT` commands for the destination key.

```bash
$ redis-cli GETBIT dest 0
```

The tester will expect the response to be `:1\r\n`.

```bash
$ redis-cli GETBIT dest 4
```

The tester will expect the response to be `:0\r\n`.

### Notes

- In this stage, you'll only need to handle `BITOP AND` with two source keys of
  the same length. Combining bitmaps of different lengths is covered in later
  stages.
- `BITOP` also supports other
  [operations](https://redis.io/docs/latest/commands/bitop/) like `OR`, `XOR`,
  and `NOT`. The `BITOP OR` operation is covered in later stages. We won't deal
  with `XOR` and `NOT` in this challenge.
