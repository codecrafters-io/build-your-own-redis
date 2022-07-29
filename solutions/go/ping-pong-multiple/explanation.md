Stage 3 builds on our progress from Stage 2.

Earlier, we were anticipating a single `PING` request, and were accordingly responding back with a single `PONG`.

As an improvement, we'll now monitor for more incoming requests — and each time we get one, we'll respond back with
`PONG`, and go back to waiting for the next one. We can achieve this with a `for` loop.

```go
for {
    if _, err := conn.Read([]byte{}); err != nil {
        fmt.Println("Error reading from client: ", err.Error())
        continue
    }
    
    conn.Write([]byte("+PONG\r\n"))
}
```

While this approach gets us through the stage, notice how our program is now blocked on the for loop as it waits 
for incoming requests. During this time, the program cannot do anything else (including accepting connections from 
other clients). We'll get around that in the next stage when we tackle concurrent requests.

Another thing to note here is that `conn.Read()` isn't guaranteed to process commands independently. `conn.Read()` 
returns bytes as and when they're available, so it's possible that the function returns with a single byte. We'll 
fix this when we implement a proper Redis protocol parser in stage 5.
