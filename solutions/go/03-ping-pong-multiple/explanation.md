Stage 3 builds on our progress from Stage 2.

Earlier, we were anticipating a single `PING` request, and were accordingly responding back with a single `PONG`.

As an improvement, we'll now monitor for more incoming requests — and each time we get one, we'll respond back with
`PONG`, and go back to waiting for the next one. We can achieve this with a `for` loop.

```go
for {
    buf := make([]byte, 1024)

    if _, err := conn.Read(buf); err != nil {
        if err == io.EOF {
            break
        } else {
            fmt.Println("error reading from client: ", err.Error())
            os.Exit(1)
        }
    }

    // Let's ignore the client's input for now and hardcode a response.
    // We'll implement a proper Redis Protocol parser in later stages.
    conn.Write([]byte("+PONG\r\n"))
}
```

While this approach gets us through the stage, notice how our program is now blocked on the for loop as it waits 
for incoming requests. During this time, the program cannot do anything else (including accepting connections from 
other clients). We'll get around that in the next stage when we tackle concurrent requests.
