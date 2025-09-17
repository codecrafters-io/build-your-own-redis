In this stage, you'll implement a TCP server that listens on port 6379.

### TCP (Transmission Control Protocol)

[TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol) is the underlying protocol used by protocols like HTTP, SSH, and others you're probably familiar with. Redis also uses TCP for communication between its clients and servers.

For this stage, your program should act as a basic TCP server that listens for incoming connections on port 6379.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then attempt to connect to your TCP server on port 6379. If the connection succeeds, you’ll pass this stage.

### Notes

- Port 6379 is the default port used by Redis.
- If you already have a Redis server running on your machine, you’ll see a “port already in use” error. To fix this, stop the existing Redis server before running your code.

{{#reader_is_bot}}
- In this stage, you can assume that you only need to handle a single client. We'll get to handling multiple clients & multiple requests per client in later stages.
{{/reader_is_bot}}
