Your `INCR` command handles integers and missing keys. In this stage, you'll
return an error when the existing value isn't a valid integer, leaving that
value unchanged.

### Recap

The implementation of [`INCR`](https://redis.io/docs/latest/commands/incr/) is
split into three stages:

- Key exists and has an integer value (previous stages)
- Key doesn't exist (previous stages)
- Key exists but doesn't have an integer value (**This stage**)

When a key's value isn't an integer, `INCR` returns a RESP simple error. For
example:

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

The tester will then connect to your server as a Redis client and run the
following commands:

```bash
$ redis-cli
> SET foo xyz (expecting "+OK\r\n" as the response)
> INCR foo (expecting "-ERR value is not an integer or out of range\r\n" as the response)
```
