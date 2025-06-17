package main

import "core:fmt"
import "core:net"
import "core:strings"
import "core:bytes"

main :: proc (){
    listen_socket, listen_err := net.listen_tcp(net.Endpoint{
        port = 6379,
        address = net.IP4_Loopback
    })
    if listen_err != nil {
        fmt.panicf("listen error : %s", listen_err)
    }
    client_socket, client_endpoint, accept_err := net.accept_tcp(listen_socket)
    if accept_err != nil {
        fmt.panicf("%s",accept_err)
    }
    net.close(client_socket)
}

