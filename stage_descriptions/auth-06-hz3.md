In this stage, you'll add support for responding to the `AUTH` command.

### The `AUTH` command

The [`AUTH`](https://redis.io/docs/latest/commands/auth/) command is used to authenticate the current connection with the specified user.

Example usage:

```bash
# Authentication failure
> AUTH default wrongpassword
(error) WRONGPASS invalid username-password pair or user is disabled.

# Authentication success
> AUTH default correctpassword
OK
```

The `AUTH` command responds with `+OK\r\n` if the specified password's hash matches with any of the hashes in the user's password list.

In case of wrong password, the response is a RESP simple error `WRONGPASS invalid username-password pair or user is disabled.`.

In this stage, you just need to respond to the `AUTH` command appropriately. You don't actually need to authenticate the connection. We'll get to that in the later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send a `ACL SETUSER` command, specifying the `default` user and a password.

```bash
$ redis-cli
# Expect: +OK\r\n
> ACL SETUSER default >mypassword
OK
```

The tester will validate that the response to the `ACL SETUSER` command is `+OK\r\n`.

It'll then send two `AUTH` commands, specifying the `default` user. First, it'll send a wrong password. Next, it'll send the correct password.

```bash
# Expect error starting with: WRONGPASS
> AUTH default wrongpassword
(error) WRONGPASS invalid username-password pair or user is disabled.

# Expect: +OK\r\n
> AUTH default mypassword
OK
```

The tester will validate the following for the responses to the `AUTH` command:

- In case of incorrect password, the response is a RESP simple error starting with the string `WRONGPASS`.
- In case of correct password, the response is `+OK\r\n`.

### Notes

- The tester will be lenient in checking error messages. Any authentication failure starting with `WRONGPASS` is valid. For example,
    - `WRONGPASS wrong password`
    - `WRONGPASS invalid authentication`