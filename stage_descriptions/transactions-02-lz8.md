In this stage, you'll add support for handling the `INCR` command when a key does not exist.

### Recap

The implementation of [`INCR`](https://redis.io/docs/latest/commands/incr/) is split into three stages:

- Key exists and has a numerical value (previous stages)
- Key doesn't exist (**This stage**)
- Key exists but doesn't have a numerical value (later stages)

When a key doesn't exist, `INCR` sets the value to 1. Example:

```bash
$ redis-cli INCR missing_key
(integer) 1
$ redis-cli GET missing_key
"1"
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client and run the following commands:

```bash
$ redis-cli
> INCR foo (expecting ":1\r\n" as the response)
> INCR bar (expecting ":1\r\n" as the response)
```

### Notes

- Your implementation still needs to pass the tests in previous stages.
