In this stage, you'll add support for a non-zero timeout duration for the `BZPOPMIN` command.

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send a `BZPOPMIN` command with a non-zero timeout:

```bash
$ redis-cli BZPOPMIN zset_key 0.1
# (Blocks for 0.1 seconds)
```

After the timeout expires, the tester will expect to receive a null array (`*-1\r\n`) as the response.

The tester will also test the case where a member is added to the zset before the timeout is reached. In this case, the response should be a RESP encoded array like `["zset_key", "foo", "20.3"]` where `foo` is the added member with score `20.3`.