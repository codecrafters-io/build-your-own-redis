In this stage, you'll implement a TCP server that listens on port 6379.

[TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol) is the underlying protocol used by protocols like HTTP, SSH and others
you're probably familiar with. Redis clients & servers use TCP to communicate with each other.

Don't worry if you're unfamiliar with the TCP protocol, or what Redis clients & servers are. You'll learn more about this in the
next stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then try to connect to your TCP server on port 6379. If the connection succeeds, you'll pass this stage.

### Notes

- 6379 is the default port that Redis uses.
- If you already have a Redis server running on your machine and listening on port 6379, you'll see a "port already in use" error when running your code. Try stopping the existing Redis server and running your code again.

{{#reader_is_bot}}
- In this stage, you can assume that you only need to handle a single client. We'll get to handling multiple clients & multiple requests per client in later stages.
{{/reader_is_bot}}