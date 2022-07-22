package main

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"strconv"
	"time"
)

func main() {
	l, err := net.Listen("tcp", "0.0.0.0:6379")
	if err != nil {
		fmt.Println("Failed to bind to port 6379")
		os.Exit(1)
	}

	storage := NewStorage()

	for {
		conn, err := l.Accept()
		if err != nil {
			fmt.Println("Error accepting connection: ", err.Error())
			os.Exit(1)
		}

		go handleConnection(conn, storage)
	}
}

func handleConnection(conn net.Conn, storage *Storage) {
	for {
		value, err := DecodeRESP(bufio.NewReader(conn))
		if err != nil {
			fmt.Println("Error decoding RESP: ", err.Error())
			return // Ignore clients that we fail to read from
		}

		command := value.Array()[0].String()
		args := value.Array()[1:]

		switch command {
		case "ping":
			conn.Write([]byte("+PONG\r\n"))
		case "echo":
			conn.Write([]byte(fmt.Sprintf("$%d\r\n%s\r\n", len(args[0].String()), args[0].String())))
		case "set":
			if len(args) > 2 {
				if args[2].String() == "px" {
					expiryStr := args[3].String()
					expiryInMilliseconds, err := strconv.Atoi(expiryStr)
					if err != nil {
						conn.Write([]byte(fmt.Sprintf("-ERR PX value (%s) is not an integer\r\n", expiryStr)))
						break
					}

					storage.SetWithExpiry(args[0].String(), args[1].String(), time.Duration(expiryInMilliseconds)*time.Millisecond)
				} else {
					conn.Write([]byte(fmt.Sprintf("-ERR unknown option for set: %s\r\n", args[2].String())))
				}
			} else {
				storage.Set(args[0].String(), args[1].String())
			}

			conn.Write([]byte("+OK\r\n"))
		case "get":
			value, found := storage.Get(args[0].String())
			if found {
				conn.Write([]byte(fmt.Sprintf("$%d\r\n%s\r\n", len(value), value)))
			} else {
				conn.Write([]byte("$-1\r\n"))
			}
		default:
			conn.Write([]byte("-ERR unknown command '" + command + "'\r\n"))
		}
	}
}
