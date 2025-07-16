In this stage, you'll add support for sending an empty RDB file as a master. This is part of the "full resynchronization" process.

### Full resynchronization

When a replica connects to a master for the first time, it sends a `PSYNC ? -1` command. This is the replica's way of
telling the master that it doesn't have any data yet, and needs to be fully resynchronized.

The master acknowledges this by sending a `FULLRESYNC` response to the replica.

After sending the `FULLRESYNC` response, the master will then send a RDB file of its current state to the replica. The replica is expected to load the file into memory, replacing its current state.

For the purposes of this challenge, you don't have to actually construct an RDB file. We'll assume that the master's database is always empty,
and just hardcode an empty RDB file to send to the replica.

You can find the hex representation of an empty RDB file [here](https://github.com/codecrafters-io/redis-tester/blob/main/internal/assets/empty_rdb_hex.md).

The tester will accept any valid RDB file that is empty, you don't need to send the exact file above.

The file is sent using the following format:

```
$<length_of_file>\r\n<binary_contents_of_file>
```

(This is similar to how [Bulk Strings](https://redis.io/topics/protocol#resp-bulk-strings) are encoded, but without the trailing `\r\n`)

### Tests

The tester will execute your program like this:

```
./your_program.sh --port <PORT>
```

It'll then connect to your TCP server as a replica and execute the following commands:

1. `PING` (expecting `+PONG\r\n` back)
2. `REPLCONF listening-port <PORT>` (expecting `+OK\r\n` back)
3. `REPLCONF capa eof capa psync2` (expecting `+OK\r\n` back)
4. `PSYNC ? -1` (expecting `+FULLRESYNC <REPL_ID> 0\r\n` back)

After receiving a response to the last command, the tester will expect to receive an empty RDB file from your server.

### Notes

- The [RDB file link](https://github.com/codecrafters-io/redis-tester/blob/main/internal/assets/empty_rdb_hex.md) contains hex & base64 representations
  of the file. You need to decode these into binary contents before sending it to the replica.
- The RDB file should be sent like this: `$<length>\r\n<contents>`
  - `<length>` is the length of the file in bytes
  - `<contents>` is the binary contents of the file
  - Note that this is NOT a RESP bulk string, it doesn't contain a `\r\n` at the end
- If you want to learn more about the RDB file format, read [this blog post](https://rdb.fnordig.de/file_format.html). This challenge
  has a separate extension dedicated to reading RDB files.
