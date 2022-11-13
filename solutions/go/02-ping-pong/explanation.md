Stage 2 is an exercise in reading and responding to requests with `net`. The official documentation [is here](https://pkg.go.dev/net).

In the first stage, we were simply accepting a connection (`_`), but weren't doing anything with it.

```go
_, err = l.Accept()
```

As we go further, we want to actually parse the incoming request, and respond suitably. Since we know that the 
client only sends us `PING` at the moment, we can hardcode our response.

First, we store the connection into a variable so we can read its value.

```go
conn, err := l.Accept()
```

For this stage, we know that the tester _only_ sends us `PING`, so we don't have to parse the incoming data.

As mentioned in the stage instructions, we need to encode the response as a 
[RESP Simple String](https://redis.io/docs/reference/protocol-spec/#resp-simple-strings). The ideal approach is to 
create a RESP encoder function — but for now, since we know that our response will always be `PONG`, we can hardcode 
the response. We will create the function in the upcoming stages.

```go
conn.Write([]byte("+PONG\r\n"))
```
