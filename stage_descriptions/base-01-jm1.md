Before a Redis client can send a command, it needs a connection to your server.
TCP carries an ordered stream of bytes between two programs, and the port
identifies the service the client connects to. In this stage, you'll start a TCP
server listening on port `6379`, Redis's default port.

### Tests

The tester runs `./your_program.sh` and attempts to connect to port `6379`. Keep
your server running so it can accept that connection. You don't need to read
commands or send responses yet, and handling one client is enough for this
stage.

{{#reader_is_bot}}

- In this stage, you can assume that you only need to handle a single client.
  We'll get to handling multiple clients & multiple requests per client in later
  stages. {{/reader_is_bot}}
