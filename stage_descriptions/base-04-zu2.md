In this stage, you'll add support for multiple concurrent clients.

### Handling Multiple Clients

In addition to handling multiple commands from the same client, Redis servers can also handle multiple clients at once.

{{#lang_is_javascript}}
In most languages, you'd need to either use threads or implement an
[Event Loop](https://en.wikipedia.org/wiki/Event_loop) to do this. In JavaScript, however, since [the concurrency
model itself is based on an event loop](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop), most
standard library functions are designed to support this kind of concurrent behaviour out of the box. **It is very
likely that the code you had for previous stages will pass this stage without any changes!**
{{/lang_is_javascript}}

{{#lang_is_typescript}}
In most languages, you'd need to either use threads or implement an [Event Loop](https://en.wikipedia.org/wiki/Event_loop) to do this. In JavaScript, however, since [the concurrency
model itself is based on an event loop](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop), most standard library functions are designed to support this kind of concurrent behaviour out of the box. **It is very likely that the code you had for previous stages will pass this stage without any changes!**
{{/lang_is_typescript}}

{{^lang_is_javascript}}
  {{^lang_is_typescript}}
To implement this, you'll need to either use threads or, if you're feeling adventurous, implement an [Event Loop](https://en.wikipedia.org/wiki/Event_loop) (like the official Redis implementation does).
  {{/lang_is_typescript}}
{{/lang_is_javascript}}

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send two `PING` commands concurrently using two different connections:

```bash
# These two will be sent concurrently so that we can test your server's ability to handle concurrent clients.
$ redis-cli PING
$ redis-cli PING
```
The tester will expect to receive two separate `+PONG\r\n` responses.

### Notes

- Since the tester client _only_ sends the `PING` command at the moment, it's okay to
  ignore what the client sends and hardcode a response. We'll get to parsing
  client input in later stages.
