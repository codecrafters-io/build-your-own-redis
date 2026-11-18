Your master announces a full synchronization. In this stage, you'll follow that
response with an empty RDB snapshot so the replica has a starting dataset for
later updates.

### Full resynchronization

When a replica connects to a master for the first time, it sends a `PSYNC ? -1`
command. This is the replica's way of telling the master that it doesn't have
any data yet and needs to be fully resynchronized.

The master responds in two steps:

- It acknowledges with a `FULLRESYNC` response (Handled in previous stages)
- It sends a snapshot of its current state as an
  [RDB file](https://rdb.fnordig.de/file_format.html).

An RDB file is a binary snapshot of the database at one point in time. During
full synchronization, the replica replaces its dataset with the snapshot's
contents.

For this transfer, assume the master's dataset is empty. You can use a fixed
empty RDB file without implementing snapshot generation or completing the RDB
persistence extension.

The
[empty RDB snapshot](https://github.com/codecrafters-io/redis-tester/blob/main/internal/assets/empty_rdb_hex.md)
is provided as hex and base64 text. Decode either representation into binary
contents before sending it to the replica.

The file is sent using the following format:

```
$<length_of_file>\r\n<binary_contents_of_file>
```

This is similar to how
[bulk strings](https://redis.io/topics/protocol#resp-bulk-strings) are encoded,
but **without the trailing `\r\n`**.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It will then connect to your TCP server as a replica and execute the following
commands:

1. `PING` - expecting `+PONG\r\n`
2. `REPLCONF listening-port <PORT>` - expecting `+OK\r\n`
3. `REPLCONF capa psync2` - expecting `+OK\r\n`
4. `PSYNC ? -1` - expecting `+FULLRESYNC <REPL_ID> 0\r\n`

After the last response, the tester will expect to receive an empty RDB file
from your server.

The tester will accept any valid RDB file that is empty.

### Notes

- Count the decoded file's bytes for `<length_of_file>`. The hexadecimal or
  Base64 text has a different length and must not be sent as the file's
  contents.
- If you want to learn more about the RDB file format, read
  [the RDB file format reference](https://rdb.fnordig.de/file_format.html). This
  challenge has a separate extension dedicated to reading RDB files.
