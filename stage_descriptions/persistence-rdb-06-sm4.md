Your server restores keys and values from an RDB file. In this stage, you'll
preserve their expiry times too, so loading a snapshot doesn't make expired data
available again.

### Restoring expiry times

RDB expiry values are absolute Unix timestamps, measured from 1 January 1970 at
00:00 UTC. The `FD` marker stores seconds in four little-endian bytes, and `FC`
stores milliseconds in eight little-endian bytes. Compare each timestamp with
the current time in the same units. The earlier `SET ... PX` option specified a
lifetime measured from the write, so loading must not restart that lifetime.

Keys without an expiry remain available. Keys with a future expiry remain
available only until that timestamp.

### Tests

The tester creates an RDB file with multiple keys and expiry timestamps in the
past and future.

The tester will execute your program like this:

```bash
$ ./your_program.sh --dir <dir> --dbfilename <filename>
```

It'll then send multiple `GET <key>` commands to your server.

```bash
$ redis-cli GET "foo"
$ redis-cli GET "bar"
```

When a key has expired, the expected response is `$-1\r\n` (a "null bulk
string").

When a key hasn't expired, the expected response is a RESP bulk string with the
value corresponding to the key.
