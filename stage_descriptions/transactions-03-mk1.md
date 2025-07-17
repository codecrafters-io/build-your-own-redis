In this stage, you'll add support for handling the `INCR` command when a key exists but doesn't have a numerical value.

### Recap

The implementation of [`INCR`](https://redis.io/docs/latest/commands/incr/) is split into three stages:

- Key exists and has a numerical value (previous stages)
- Key doesn't exist (previous stages)
- Key exists but doesn't have a numerical value (**This stage**)

When a key exists but doesn't have a numerical value, `INCR` will return an error. Example:

```bash
$ redis-cli SET foo xyz
"OK"
$ redis-cli INCR foo
(error) ERR value is not an integer or out of range
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client and run the following commands:

```bash
$ redis-cli
> SET foo xyz (expecting "+OK\r\n" as the response)
> INCR foo (expecting "-ERR value is not an integer or out of range\r\n" as the response)
```
