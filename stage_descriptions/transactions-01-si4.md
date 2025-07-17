In this stage, you'll add support for the `INCR` command.

### The INCR command

The [INCR](https://redis.io/docs/latest/commands/incr/) command is used to increment the value of a key by 1.

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

- Key exists and has a numerical value (**This stage**)
- Key doesn't exist (later stages)
- Key exists but doesn't have a numerical value (later stages)

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then connect to your server as a Redis client and run the following commands:

```bash
$ redis-cli
> SET foo 41 (expecting "+OK" as the response)
> INCR foo (expecting ":42\r\n" as the response)
```
