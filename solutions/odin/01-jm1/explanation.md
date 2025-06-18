The entry point for your Redis implementation is in `src/main.odin`.

Study and uncomment the relevant code: 

```odin
// Uncomment this block to pass the first stage
listen_socket, listen_err := net.listen_tcp(net.Endpoint{
    port = 6379,
    address = net.IP4_Loopback
})
if listen_err != nil {
    fmt.panicf("%s", listen_err)
}
client_socket, client_endpoint, accept_err := net.accept_tcp(listen_socket)
if accept_err != nil {
    fmt.panicf("%s", accept_err)
}
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
