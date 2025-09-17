In this stage, you'll add support for the [ECHO](https://redis.io/commands/echo) command.

### The `ECHO` Command

The `ECHO` command is used for testing and debugging, similar to `PING`. 

```bash
$ redis-cli PING # The command you implemented in the previous stages
PONG
$ redis-cli ECHO hey # The command you'll implement in this stage
hey
```

It accepts a single argument and sends it back as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

To pass this stage, your program will need to:
- Parse the client's input to extract the argument from the `ECHO` command
- Then encode that argument as a bulk string for the response.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ECHO` command with an argument to your server:

```bash
$ redis-cli ECHO hey
```

The tester will expect to receive `$3\r\nhey\r\n` as a response. That's the string `hey` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- We highly recommend you implement a proper RESP parser at this stage. It'll come in handy in later stages.
- The exact bytes you'll receive will be a [RESP Array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays) that looks something like `*2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n`. This is the RESP encoding of `["ECHO", "hey"]`. You'll need to parse this array to get the command and its argument. 
- Redis command names are case-insensitive, so `ECHO`, `echo`, and `EcHo` are all valid commands.
- The tester will send a random string as an argument to the `ECHO` command, so you won't be able to hardcode the response to pass this stage.
- You can learn more about how commands are handled in the Redis protocol [here](https://redis.io/docs/latest/develop/reference/protocol-spec/#sending-commands-to-a-redis-server).
