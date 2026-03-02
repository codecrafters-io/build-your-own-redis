In this stage, you'll add support for the `WATCH` command.

### The `WATCH` command

[The `WATCH` command](https://redis.io/docs/latest/commands/watch/) marks a key to be monitored for changes.
If the watched key is modified by another client before the transaction is executed, the transaction will be aborted. This enables simple optimistic locking behavior in Redis.

Example Usage:
```bash
$ redis-cli WATCH key
OK
```

The response is always the simple string `"+OK\r\n"`.


### Tests
The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then send a `WATCH` command with a key.

```bash
$ redis-cli WATCH key (expecting "+OK\r\n" as the response)
```

### Notes

- In this stage, you'll only need to handle replying to the `WATCH` command.

- We will get to its implementation details in the later stages.