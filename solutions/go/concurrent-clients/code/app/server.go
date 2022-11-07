package main

import (
	"bytes"
	"fmt"
	"io"
	"net"
	"os"
)

func main() {
	l, err := net.Listen("tcp", "0.0.0.0:6379")
	if err != nil {
		fmt.Println("Failed to bind to port 6379")
		os.Exit(1)
	}

	for {
		conn, err := l.Accept()
		if err != nil {
			fmt.Println("Error accepting connection: ", err.Error())
			os.Exit(1)
		}

		go handleConnection(conn)
	}
}

func handleConnection(conn net.Conn) {
	defer conn.Close()

	for {
		buf := make([]byte, 1024)

		if _, err := conn.Read(buf); err != nil {
			if err == io.EOF {
				continue
			} else {
				fmt.Println("error reading from client: ", err.Error())
				os.Exit(1)
			}
		}

		// Let's use a simple substring check for now. We'll implement a proper Redis Protocol parser in later stages.
		if bytes.Contains(buf, []byte("ping")) {
			conn.Write([]byte("+PONG\r\n"))
		} else {
			fmt.Println("received unknown command:", string(buf))
			os.Exit(1)
		}
	}
}
