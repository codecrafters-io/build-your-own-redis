package main

import (
	"bytes"
	"fmt"
	"net"
	"os"
)

func main() {
	l, err := net.Listen("tcp", "0.0.0.0:6379")
	if err != nil {
		fmt.Println("Failed to bind to port 6379")
		os.Exit(1)
	}

	conn, err := l.Accept()
	if err != nil {
		fmt.Println("Error accepting connection: ", err.Error())
		os.Exit(1)
	}

	defer conn.Close()

	buf := make([]byte, 1024)

	if _, err := conn.Read(buf); err != nil {
		fmt.Println("error reading from client: ", err.Error())
		os.Exit(1)
	}

	// Let's ignore the client's input for now and hardcode a response.
	// We'll implement a proper Redis Protocol parser in later stages.
	conn.Write([]byte("+PONG\r\n"))
}
