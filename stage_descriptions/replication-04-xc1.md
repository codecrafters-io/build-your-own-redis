In this stage, you'll extend your `INFO` command to return two additional values: `master_replid` and `master_repl_offset`.

### The Replication ID and Offset

Every Redis master has a **replication ID**: a 40-character pseudo-random alphanumeric string that identifies the replication stream. This ID is generated when the master starts and resets each time the master restarts from scratch.

Each master also maintains a **replication offset** that tracks how many bytes of commands have been sent to replicas. The offset starts at `0` when a master boots up and no replicas have connected yet.

In this stage, you'll initialize a replication ID and replication offset for the master server:

- The ID can be any pseudo-random alphanumeric string of 40 characters.
  - For the purposes of this challenge, you don't need to generate a random string. You can hardcode it instead.
  - As an example, you can hardcode `8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb` as the replication ID.
- The offset should be `0`.

These two values should be returned as part of the `INFO` command output, under the `master_replid` and `master_repl_offset` keys, respectively.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send the `INFO` command with the `replication` option to your server.

```bash
$ redis-cli INFO replication
```

Your program should respond with a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings) where each line is a key-value pair separated by a colon (`:`). The tester will look for the following key-value pairs:

- `role`: `master`
- `master_replid`: A 40-character alphanumeric string
- `master_repl_offset`: `0`

### Notes

- Your code must pass previous stage tests, meaning you should still return the correct `role` key.
