In this stage, you'll implement enforcing authentication using the `AUTH` command.

### Enforcing authentication using `AUTH`

After the `AUTH` command returns success, the connection is authenticated as the specified user. For example,

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

> AUTH default mypassword
OK

# Client 2 is now authenticated as the 'default' user
> ACL WHOAMI
"default"
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send commands to two different clients.

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

The tester will validate the following:

1. A new client receives a `NOAUTH` error when attempting to execute commands before authenticating.

2. The `AUTH` command returns `OK` upon successful authentication.

3. The client can execute commands successfully after authentication.