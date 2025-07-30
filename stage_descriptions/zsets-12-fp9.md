In this stage, you'll add support for the `BZPOPMIN` command, which blocks until a member is available to be removed from a zset.

### The `BZPOPMIN` Command

`BZPOPMIN` is a blocking variant of the `ZPOPMIN` command. It allows clients to wait for a member to become available on a zset. If the zset is empty, the command blocks until:

- A member is added to the zset
- Or the specified timeout is reached (in seconds)
    - It blocks indefinitely if the timeout specified is 0.

Example usage:

```bash
# Here the timeout is specified as 0 (i.e. wait indefinitely)
> BZPOPMIN zset_key 0

# ... this blocks until a member is added to the zset

# As soon as a member is added, it responds with a resp array:
1) "zset_key"
2) "foo"
3) "20.0"
```

If a timeout duration is supplied, it is the number of seconds the client will wait for a member to be available for removal. If no members were added during this interval, the server returns a null array (`*-1\r\n`).

If a member was added during this interval, the server removes it from the zset and responds to the blocking client with a RESP-encoded array containing three elements:

1. The zset name (as a bulk string)
2. The element that was popped (as a bulk string)
3. The score of the element that was popped (as a bulk string)

If multiple clients are blocked for `BZPOPMIN` command, the server responds to the client which has been blocked for the longest duration.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send a `BZPOPMIN` command with the timeout set to `0`:

```bash
$ redis-cli BZPOPMIN zset_key 0
# (Blocks)
```

After a short while, it'll then send `ZADD` using another client:

```bash
# In another client:
$ redis-cli ZADD zset_key 10.0 "foo"
# Expect: (integer) 1
```

The tester will then expect the following response from the first client:

```bash
# RESP encoding of ["zset_key", "foo", "10.0"] ->
*3\r\n
$8\r\n
zset_key\r\n
$3\r\n
foo\r\n
$4\r\n
10.0\r\n
```

The tester will also test `BZPOPMIN` using multiple blocking clients. For example, it will spawn multiple clients one after another, and send `BZPOPMIN` command from each client specifying the same zset.

```bash
# Client 1 sends BZPOPMIN first
$ redis-cli BZPOPMIN another_zset 0

# Client 2 sends BZPOPMIN second
$ redis-cli BZPOPMIN another_zset 0
```

It will then spawn a separate client which will send `ZADD` to the server specifying the zset.

```bash
# Client 3 adds a new member to the zset
$ redis-cli ZADD another_zset 100.0 "foo"
```

The tester will expect the response to be sent to Client 1 since it has been blocked for the longest time.

### Notes

- In this stage, the timeout argument will always be 0, i.e. `BZPOPMIN` should wait indefinitely. We'll get to non-zero timeouts in the next stage. 