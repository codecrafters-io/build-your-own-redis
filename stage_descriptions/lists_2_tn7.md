In this stage, you'll add support for `RPUSH` when a list already exists and a single element is being appended.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send multiple `RPUSH` commands specifying the same list.

```bash
$ redis-cli RPUSH list_key "element1"
# Expect: (integer) 1 → encoded as :1\r\n

$ redis-cli RPUSH list_key "element2"
# Expect: (integer) 2 → encoded as :2\r\n
```

In each case, the tester will expect the response to be the length of the list as a RESP encoded integer. 