Stage 2 is an exercise in reading and responding to requests using [net.Socket objects](https://nodejs.org/api/net.html#class-netsocket).

In the first stage, we were simply accepting a connection but weren't reading data from it.

As we go further, we want to actually parse the incoming request, and respond suitably.

First, we'll add an event listener for the `data` event.

```javascript
connection.on("data", () => {
  // Respond with PONG
});
```

Next, we'll send our response to the client. For this stage, we know that the tester _only_ sends us `PING`, so we don't have to
parse the incoming data. We can hardcode `PONG` as our response.

As mentioned in the stage instructions, we need to encode the response as a
[RESP Simple String](https://redis.io/docs/reference/protocol-spec/#resp-simple-strings). The ideal approach is to
create a RESP encoder function — but for now, since we know that our response will always be `PONG`, we can hardcode
the response. We will create the function in the upcoming stages.

```python
connection.write("+PONG\r\n");
```
