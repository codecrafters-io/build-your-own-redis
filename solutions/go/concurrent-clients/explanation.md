Stage 4 is an entry into Goroutines. To get a primer, [read here](https://gobyexample.com/goroutines).

We're now going to respond to `PING` requests from multiple clients, concurrently.

Let's start by moving our responder into its own function. It needs a connection object as argument.

```go
func handleConnection(conn net.Conn) {
    for {
        if _, err := conn.Read([]byte{}); err != nil {
            fmt.Println("Error reading from client: ", err.Error())
            continue
        }
        
        conn.Write([]byte("+PONG\r\n"))
    }
}
```

Within `main`, we'll now use a `for` to accept multiple connections. For each client connection that we accept, we'll 
spawn a goroutine that handles the connection.

Using a goroutine makes the program able to accept and handle new incoming connections in parallel, instead of 
blocking the program by sequentially handling one connection at a time.

```go
for {
    conn, err := l.Accept()
    if err != nil {
        fmt.Println("Error reading from client: ", err.Error())
        os.Exit(1)
    }

    go handleConnection(conn)
}
```
