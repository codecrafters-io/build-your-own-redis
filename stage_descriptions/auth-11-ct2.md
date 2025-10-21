In this stage, you'll add support for retrieving the `default` user's information using the `ACL GETUSER` command.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL GETUSER` command to retrieve the `default` user's information.

```bash
$ redis-cli
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
    2) "nopass"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "+@all"
```

The tester will validate that the response is an array with the following structure:

```

```

### Notes

- The `nopass` flag means that a password is not required to authenticate as the given user.