In this stage, you'll add support for disallowing the `WATCH` command in a transaction.

### `WATCH` in transaction

[The `WATCH` command](https://redis.io/docs/latest/commands/watch/) is not allowed inside a transaction. If a `WATCH` command is issued from a client when it has begun a transaction using the `MULTI` command, the response to the `WATCH` is an error.

Example Usage:
```
$ redis-cli
> MULTI
OK
(TX)> WATCH foo
(error) ERR WATCH inside MULTI is not allowed
```

### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then begin a transaction using the `MULTI` command, and send a `WATCH` command with a random key.

```bash
$ redis-cli
> MULTI (expecting "+OK" as the response)

> WATCH key
# Expect: (error) ERR WATCH inside MULTI is not allowed
```
The response is a RESP simple string, which is encoded as:
```
-ERR WATCH inside MULTI is not allowed\r\n
```

The tester will then abort the transaction using the `DISCARD` command, and again send a `WATCH` command. It will expect the response to be `"+OK\r\n` in this case. 

```bash
> DISCARD (expecting "+OK\r\n" as the response)

> WATCH key2 (expecting "+OK\r\n" as the response)
```

