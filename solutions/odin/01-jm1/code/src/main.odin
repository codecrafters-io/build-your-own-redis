package main

import "core:fmt"
import "core:net"
import "core:os"

main :: proc() {
    listener, err := net.listen_tcp(net.Endpoint{address = net.IP4_Any, port = 6379})
    if err != nil {
        fmt.eprintln("Failed to bind to port 6379")
        os.exit(1)
    }
    client, _, accept_err := net.accept_tcp(listener)
    if accept_err != nil {
        fmt.eprintln("Error accepting connection")
        os.exit(1)
    }
}
