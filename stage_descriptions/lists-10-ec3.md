In this stage, you'll add support for the `BLPOP` command, which blocks until an element is available to be popped.

### The `BLPOP` Command

[`BLPOP`](https://redis.io/docs/latest/commands/blpop/) is a blocking variant of the `LPOP` command. It lets a client wait for an element to become available on a list before popping it.

If the list is empty, the command blocks until:

- A new element is added to the list
- Or a specified timeout is reached.
  
If the timeout is `0`, the command blocks indefinitely.

For example, a client can block on a list with an indefinite timeout like this:

```bash
> BLPOP list_key 0
```

This client will wait until an element is added to `list_key`. When an element like `"foobar"` is added, it is removed from the list, and the server responds with a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays):

```bash
1) "list_key"
2) "foobar"
```

The RESP array contains two bulk strings:
1. The list name
2. The element that was popped

The `BLPOP` command has a few other behaviours to keep in mind:

- If a timeout duration is supplied, it is the number of seconds the client will wait for an element. 
- If no elements were inserted during the timeout interval, the server returns a [null bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings) (`$-1\r\n`).
- If multiple clients are blocked for the same list, the server responds to the client that has been waiting the longest.

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

After a short while, another client will push an element to the same list:

```bash
# In another client:
$ redis-cli RPUSH list_key "foo"
# Expect: (integer) 1
```

The tester expects the first client to immediately receive the response, which is the RESP-encoded array `["list_key", "foo"]`.

```bash
# RESP encoding of ["list_key", "foo"]
*2\r\n
$8\r\n
list_key\r\n
$3\r\n
foo\r\n
```

The tester will also test `BLPOP` using multiple blocking clients. It will spawn several clients and have each one send a `BLPOP` command to the same list.

```bash
# Client 1 sends BLPOP first
$ redis-cli BLPOP another_list_key 0

# Client 2 sends BLPOP second
$ redis-cli BLPOP another_list_key 0
```

Then, a separate client will use `RPUSH` to add an element to the list.

```
$ redis-cli RPUSH list_key "foo"
```

The tester will expect your server to respond to the client that sent the `BLPOP` command first (`Client 1`).

### Notes

- In this stage, the timeout argument will always be `0`, meaning `BLPOP` should wait indefinitely. We'll handle non-zero timeouts in a later stage.
