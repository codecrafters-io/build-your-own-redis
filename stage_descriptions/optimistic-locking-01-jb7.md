In this stage, you'll add support for the `WATCH` command.

### Optimistic Locking in Redis

Say two clients both want to increment a counter that's currently set to `10`. Client A reads `10`, Client B reads `10`. Client A writes `11`. Client B also writes `11`. The second write overwrites the first, so only one increment sticks while the other update gets lost.

This is the "lost update problem", and it shows up whenever multiple clients read-then-write the same key. The classic fix is to lock the key before reading it so nobody else can touch it until you're done (pessimistic locking). That works, but locks are expensive. If most updates don't actually collide, you're paying the cost of locking every time for conflicts that rarely happen.

Optimistic locking flips the approach. Instead of locking upfront, you let every client read and write freely, but you keep track of what they read. Right before a client commits its changes, you check: did any of those values change since the client read them? If so, the commit is rejected, and the client can retry with fresh data.

### The `WATCH` Command

The [`WATCH` command](https://redis.io/docs/latest/commands/watch/) is how Redis implements optimistic locking. It tells the server to monitor a key for changes. Later, when the client executes a group of commands as a [transaction](https://redis.io/docs/latest/develop/using-commands/transactions/), the server checks whether any watched keys were modified in the meantime. If they were, the whole transaction is discarded.

Here's what that looks like in practice:

```bash
Client A: WATCH counter          # "Tell me if this changes"
Client A: GET counter → 10       # Read current value
Client B: SET counter 20         # Another client sneaks in an update
Client A: MULTI                  # Start a transaction
Client A: SET counter 11         # Queue an update based on stale data
Client A: EXEC                   # Try to execute...
→ (nil)                          # Transaction aborted, counter changed
```

You'll implement this full flow in later stages. For this stage, you only need to parse the `WATCH` command and respond with `+OK\r\n`.

```bash
$ redis-cli WATCH key
OK
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send a `WATCH` command:

```bash
$ redis-cli WATCH key
```

The tester will verify that:
- The response is `+OK\r\n` (a RESP simple string)
- Your program doesn't crash or produce errors

### Notes
- `WATCH` can accept multiple keys in a single command (e.g., `WATCH key1 key2 key3`), but you only need to handle a single key for now. We'll handle multiple keys in later stages.
- The actual watch tracking and transaction abort behavior will come in later stages.
