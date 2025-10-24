In this stage, you'll add support for responding to the `ACL SETUSER` command.

### The `ACL SETUSER` command

The [`ACL SETUSER`](https://redis.io/docs/latest/commands/acl-setuser/) command is used to create a new user, or modify the properties of the existing user in the Redis' ACL system.

Example usage:

```bash
# Creating a new user
> ACL SETUSER john
OK

# Modifying the existing user's password
>ACL SETUSER john >johnspassword
OK
```

It returns OK, encoded as a [RESP simple string](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send a `ACL SETUSER` command specifying a random username.

```bash
$ redis-cli ACL SETUSER username-101 
# Expect: +OK\r\n
OK
```

The tester will validate that the response is `+OK\r\n`.

### Notes

- In this stage, you can hardcode the response to be `+OK\r\n`. We'll get to the actual implementation of `ACL SETUSER` command in the later stages.