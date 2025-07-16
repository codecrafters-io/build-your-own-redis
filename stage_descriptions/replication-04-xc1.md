In this stage, you'll extend your `INFO` command to return two additional values: `master_replid` and `master_repl_offset`.

### The replication ID and offset

Every Redis master has a replication ID: it is a large pseudo random string. This is set when the master is booted. Every time
a master instance restarts from scratch, its replication ID is reset.

Each master also maintains a "replication offset" corresponding to how many bytes of commands have been added to the replication
stream. We'll learn more about this offset in later stages. For now, just know that the value starts from `0` when a master is
booted and no replicas have connected yet.

In this stage, you'll initialize a replication ID and offset for your master:

- The ID can be any pseudo random alphanumeric string of 40 characters.
  - For the purposes of this challenge, you don't need to actually generate a random string, you can hardcode it instead.
  - As an example, you can hardcode `8371b4fb1155b71f4a04d3e1bc3e18c4a990aeeb` as the replication ID.
- The offset is to be 0.

These two values should be returned as part of the INFO command output, under the `master_replid` and `master_repl_offset` keys respectively.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It'll then send the `INFO` command with `replication` as an argument to your server.

```bash
$ redis-cli INFO replication
```

Your program should respond with a [Bulk string](https://redis.io/docs/reference/protocol-spec/#bulk-strings) where each line
is a key value pair separated by `:`. The tester will look for the following keys:

- `master_replid`, which should be a 40 character alphanumeric string
- `master_repl_offset`, which should be `0`

### Notes

- Your code should still pass the previous stage tests, so the `role` key still needs to be returned
