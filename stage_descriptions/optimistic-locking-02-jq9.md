In this stage, you'll add support for disallowing the `WATCH` command in a transaction.

### `WATCH` inside transaction

[The `WATCH` command](https://redis.io/docs/latest/commands/watch/) is not allowed inside a transaction. If a `WATCH` command is issued from a client when it has begun a transaction using the `MULTI` command, the response to the `WATCH` is an error.

Example Usage:
```bash
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

The tester will spawn a client, and begin a transaction using the `MULTI` command, and send a `WATCH` command with a random key.

```bash
$ redis-cli
> MULTI (expecting "+OK" as the response)

> WATCH key
# Expect: (error) ERR WATCH inside MULTI is not allowed
```

The response is a RESP error, which is encoded as:
```bash
-ERR WATCH inside MULTI is not allowed\r\n
```

### Notes

- The tester is lenient in checking error messages so you don't have to stick to the exact format Redis uses. It is enough for the error message to satisfy the following conditions: 
    - Start with `ERR`
    - Must include `WATCH`
    - Must include `inside multi`
    - Must include `not allowed`
 
- In this stage, you'll only need to disallow the `WATCH` command inside a transaction. We'll get to its implementation details in the later stages.

