Stage 4 is an entry into Goroutines. To get a primer, [read here](https://gobyexample.com/goroutines).

We're now going to respond to `PING` requests from multiple clients, concurrently.

Let's start by moving our responder into its own function. It needs a connection object as argument.

```go
func handleConnection(conn net.Conn) {
	defer conn.Close()

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
        fmt.Println("Error accepting connection: ", err.Error())
        os.Exit(1)
    }

    go handleConnection(conn)
}
```
