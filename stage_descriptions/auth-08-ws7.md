In this stage, you'll implement enforcing authentication using the `AUTH` command.

### Enforcing Authentication using `AUTH`

After the `AUTH` command succeeds, the connection becomes authenticated as the specified user. Once authenticated, the connection can execute commands that previously returned `NOAUTH` errors:

For example:

```bash
# Client 1
$ redis-cli
> ACL SETUSER default >newpassword
OK

> ACL WHOAMI
"default"

# Client 2 (new connection)
$ redis-cli
> ACL WHOAMI
(error) NOAUTH Authentication required.

> AUTH default newpassword
OK

# Client 2 is now authenticated as the 'default' user
> ACL WHOAMI
"default"
```

The authentication lasts for the entire connection, so the client does not need to re-authenticate for every command.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send commands to two different clients:

```bash
# Client 1
$ redis-cli
# Expect: +OK\r\n
> ACL SETUSER default >newpassword
OK

# Expect RESP bulk string: "default"
> ACL WHOAMI
"default"

# Client 2 (new connection)
$ redis-cli
# Expect error starting with: "NOAUTH"
> ACL WHOAMI
(error) NOAUTH Authentication required.

# Expect: +OK\r\n
> AUTH default mypassword
OK

# Expect RESP bulk string: "default"
> ACL WHOAMI
"default"
```

The tester will verify that:

1. A new client receives a `NOAUTH` error when attempting to execute commands before authenticating.
2. The `AUTH` command returns `OK` as a simple string on successful authentication.
3. The client can execute `ACL WHOAMI` successfully after authentication.
