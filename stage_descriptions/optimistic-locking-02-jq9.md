In this stage, you'll implement `MULTI` and update `WATCH` to reject calls inside transactions.

### Redis Transactions

In previous stages, you saw how `WATCH` fits into a larger flow: watch a key, start a transaction with `MULTI`, queue some commands, and execute with `EXEC`. Now you'll start building the transaction side.

[Redis transactions](https://redis.io/docs/latest/develop/using-commands/transactions/) allow clients to execute multiple commands as a single operation. The basic flow is:

1. `MULTI` - Enter transaction mode (commands get queued instead of executing immediately)
2. `SET key value`, `INCR counter`, etc. - Commands are queued
3. `EXEC` - Execute all queued commands

We'll implement `EXEC` in later stages. For this stage, you'll implement the `MULTI` command and make `WATCH` check whether it's being called inside a transaction.

### The `MULTI` Command

The [`MULTI` command](https://redis.io/docs/latest/commands/multi/) is used to start a transaction.

```bash
$ redis-cli MULTI
OK
```

When a client uses `MULTI`, the server:
1. Marks the connection as being inside a transaction
2. Returns `OK` as a simple string

### The `WATCH` Command (Updated)

`WATCH` is meant to be called **before** a transaction starts. Calling it inside a transaction is not allowed because the transaction has already begun, and commands are already being queued.

Now that you have the transaction state, `WATCH` needs to enforce this:

- If the connection is outside a transaction:
    1. Add the key to the connection's collection of watched keys
    2. Return `OK` as a simple string
- If the connection is inside a transaction (after `MULTI`):
    1. Return a RESP error like: `-ERR WATCH inside MULTI is not allowed\r\n`. The exact wording is flexible but should include: `ERR`, `WATCH`, `inside MULTI`, and `not allowed`.

For example:
```bash
$ redis-cli
> WATCH counter
OK
> MULTI
OK
(TX)> WATCH another_key
(error) ERR WATCH inside MULTI is not allowed
```

### Tests

The tester will execute your program like this:
```bash
$ ./your_program.sh
```

It will then spawn a client, begin a transaction using the `MULTI` command, and send a `WATCH` command with a random key:
```bash
$ redis-cli
> MULTI
OK
> WATCH key
(error) ERR WATCH inside MULTI is not allowed
```

The tester will verify that:
- `MULTI` returns `OK` as a simple string
- `WATCH` inside a transaction returns a RESP error containing: `ERR`, `WATCH`, `inside MULTI`, and `not allowed`

### Notes

- You'll need a way to track state per connection (like a map keyed by the connection/socket).
- When `WATCH` is called outside a transaction, you should still track the watched keys even though they're not used yet. This prepares you for later stages.
- The exact error message format is flexible as long as it includes the required keywords.
