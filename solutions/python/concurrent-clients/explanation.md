Stage 4 is an entry into using threads in Python. To get a primer, [read here](https://realpython.com/intro-to-python-threading/).

We're now going to respond to `PING` requests from multiple clients, concurrently.

Let's start by moving our responder into its own function. It needs a connection object as argument.

```go
def handle_connection(client_connection):
    while True:
        try:
            client_connection.recv(1024)  # wait for client to send data
            client_connection.send(b"+PONG\r\n")
        except ConnectionError:
            break  # Stop serving if the client connection is closed
```

Note that we've also added some error handling so that the loop terminates if a client disconnects.

Within `main`, we'll now use a `while` loop to accept multiple connections. For each client connection that we accept, 
we'll spawn a thread that handles the connection.

Using threads makes the program able to accept and handle new incoming connections in parallel, instead of 
blocking the program by sequentially handling one connection at a time.

```go
while True:
    client_connection, _ = server_socket.accept()  # wait for client
    threading.Thread(target=handle_connection, args=(client_connection,)).start()
```
