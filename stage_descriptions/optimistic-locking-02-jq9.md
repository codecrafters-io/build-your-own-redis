In this stage, you'll implement `MULTI` and update `WATCH` to reject calls inside transactions.

### Redis Transactions

Redis transactions allow clients to execute multiple commands as a single atomic operation. The basic flow is:

1. `MULTI` - Enter transaction mode (commands get queued instead of executing immediately)
2. `SET key value`, `INCR counter`, etc. - Commands are queued
3. `EXEC` - Execute all queued commands atomically

For this stage, you'll implement a basic `MULTI` command and make `WATCH` check whether it's being called inside a transaction.

### The `MULTI` Command

The [`MULTI` command](https://redis.io/docs/latest/commands/multi/) is used to start a transaction.

For example:

```bash
> MULTI
OK
```

When `MULTI` is called, the server:
1. Marks the connection as being inside a transaction
2. Returns `OK` as a simple string

### The WATCH Command (Updated)

Now that you have transaction state, `WATCH` needs to behave differently depending on whether the connection is inside a transaction:

- If the connection is outside a transaction:
    1. Add the key to the connection's collection of watched keys
    2. Return `OK` as a simple string
- If the connection is inside a transaction (after `MULTI`):
    1. Return an error: `-ERR WATCH inside MULTI is not allowed\r\n`

For example:
```bash
> WATCH counter
OK
> MULTI
OK
> WATCH another_key
(error) ERR WATCH inside MULTI is not allowed
```

### Tests

The tester will execute your program like this:
```bash
$ ./your_program.sh
```

It will then spawn a client, and begin a transaction using the `MULTI` command, and send a `WATCH` command with a random key:
```bash
> MULTI
OK
> WATCH key
(error) ERR WATCH inside MULTI is not allowed
```

The tester will verify that:
- `MULTI` returns `+OK\r\n`
- `WATCH` inside a transaction returns a RESP error containing: `ERR`, `WATCH`, `inside MULTI`, and `not allowed`

### Notes

- You'll need a way to track state per connection (like a map keyed by the connection/socket).
- When `WATCH` is called outside a transaction, you should still track the watched keys even though they're not used yet. This prepares you for later stages.
- The exact error message format is flexible as long as it includes the required keywords.
