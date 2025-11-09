In this stage, you'll add support for the `AUTH` command.

### The `AUTH` Command

The [`AUTH`](https://redis.io/docs/latest/commands/auth/) command authenticates the current connection with a specified username and password.

The command format is:

```bash
AUTH <username> <password>
```

For example:

```bash
# Authentication failure
> AUTH default wrongpassword
(error) WRONGPASS invalid username-password pair or user is disabled.

# Authentication success
> AUTH default correctpassword
OK
```

The server responds with `+OK\r\n` if the specified password's hash matches any of the hashes in the user's password list.

If no match is found, the server responds with the [RESP simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors): `WRONGPASS invalid username-password pair or user is disabled.`

For this stage, you just need to respond to the `AUTH` command appropriately. You don't need to actually authenticate the connection. We'll get to that in later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `ACL SETUSER` command, specifying the `default` user and a password:

```bash
$ redis-cli ACL SETUSER default >mypassword
OK
```

Your server should respond with `OK` encoded as a simple string (`+OK\r\n`).

Next, the tester will send two `AUTH` commands: one with a wrong password and another with a correct password.

```bash
# Expect error starting with: WRONGPASS
> AUTH default wrongpassword
(error) WRONGPASS invalid username-password pair or user is disabled.

# Expect: +OK\r\n
> AUTH default mypassword
OK
```

The tester will verify that:
- Wrong passwords return a [simple error](https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors) starting with `WRONGPASS`.
- Correct passwords return `+OK\r\n`.

### Notes

- The tester will be lenient in checking error messages. Any authentication failure starting with `WRONGPASS` is valid. For example:
    - `WRONGPASS wrong password`
    - `WRONGPASS invalid authentication`
