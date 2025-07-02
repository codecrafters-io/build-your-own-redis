In this stage, you'll add support for the `BLPOP` command, which blocks until an element is available to be popped.

### The `BLPOP` Command

`BLPOP` is a blocking variant of the `LPOP` command. It allows clients to wait for an element to become available on one or more lists. If the list is empty, the command blocks until:

- An element is pushed to the list
- Or the specified timeout is reached (in seconds)
    - It blocks indefinitely if the timeout specified is 0.

Example usage:

```bash
# Here the timeout is specified as 0 (i.e. wait indefinitely)
> BLPOP list_key 0

# ... this blocks until an element is added to the list

# As soon as an element is added, it responds with a resp array:
1) "list_key"
2) "foobar"
```

If a timeout duration is supplied, it is the number of seconds the client will wait for an element to be available for removal. If no elements were inserted during this interval, the server returns a null bulk string (`$-1\r\n`).

If an element was inserted during this interval, the server removes it from the list and responds to the blocking client with a RESP-encoded array containing two elements:

1. The list name (as a bulk string)
1. The element that was popped (as a bulk string)

If multiple clients are blocked for `BLPOP` command, the server responds to the client which has been blocked for the longest duration.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send a `BLPOP` command with the timeout set to `0`:

```bash
$ redis-cli BLPOP list_key 0
# (Blocks)
```

After a short while, it'll then send `RPUSH` using another client:

```bash
# In another client:
$ redis-cli RPUSH list_key "foo"
# Expect: (integer) 1
```

The tester will then expect the following response from the first client:

```bash
# RESP encoding of ["list_key", "foo"] ->
*2\r\n
$8\r\n
list_key\r\n
$3\r\n
foo\r\n
```

The tester will also test `BLPOP` using multiple blocking clients. For example, it will spawn multiple clients one after another, and send `BLPOP` command from each client specifying the same list.

```bash
# Client 1 sends BLPOP first
$ redis-cli BLPOP another_list_key 0

# Client 2 sends BLPOP second
$ redis-cli BLPOP another_list key 0
```

It will then spawn a separate client which will send `RPUSH` to the server specifying the list.

```
$ redis-cli RPUSH list_key "foo"
```

The tester will expect the response to be sent to Client 1 since it has been blocked for the longest time.

### Notes

- In this stage, the timeout argument will always be 0, i.e. BLPOP should wait indefinitely. We'll get to non-zero timeouts in later stages. 