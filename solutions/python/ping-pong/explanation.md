Stage 2 is an exercise in reading and responding to requests using a [Socket object](https://docs.python.org/3/library/socket.html#socket-objects).

In the first stage, we were simply accepting a connection (`_`), but weren't doing anything with it.

```python
server_socket.accept() # wait for client
```

As we go further, we want to actually parse the incoming request, and respond suitably. Since we know that the
client only sends us `PING` at the moment, we can hardcode our response.

First, we store the connection into a variable so we can read its value.

```python
client_connection, _ = server_socket.accept()  # wait for client
```

Next, we read data from the connection. For this stage, we know that the tester _only_ sends us `PING`, so we don't have to
parse the incoming data.

```python
client_connection.recv(1024)  # wait for client to send data
```

Once we've read data, we need to respond back with `PONG`.

As mentioned in the stage instructions, we need to encode the response as a
[RESP Simple String](https://redis.io/docs/reference/protocol-spec/#resp-simple-strings). The ideal approach is to
create a RESP encoder function — but for now, since we know that our response will always be `PONG`, we can hardcode
the response. We will create the function in the upcoming stages.

```python
client_connection.send(b"+PONG\r\n")
```
