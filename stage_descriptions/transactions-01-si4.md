This extension adds transactions, which let a client group commands for
execution without another client's commands running between them. `INCR` changes
a key's value, helping demonstrate when queued commands take effect. In this
stage, you'll implement `INCR` for an existing integer value.

### The `INCR` command

The [INCR](https://redis.io/docs/latest/commands/incr/) command interprets a
key's string value as an integer and increases it by `1`. The stored result
remains a string. The response is a RESP integer, written as `:<number>\r\n`.

Example usage:

```bash
$ redis-cli SET foo 5
"OK"
$ redis-cli INCR foo
(integer) 6
$ redis-cli INCR foo
(integer) 7
```

If the key doesn't exist, the value will be set to 1.

We'll split the implementation of this command into three stages:

- Key exists and has an integer value (**This stage**)
- Key doesn't exist (later stages)
- Key exists but doesn't have an integer value (later stages)

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client and run the
following commands:

```bash
$ redis-cli
> SET foo 41 (expecting "+OK\r\n" as the response)
> INCR foo (expecting ":42\r\n" as the response)
```
