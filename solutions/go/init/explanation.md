The entry point for your Redis implementation is in `server.go`.

Study and uncomment the relevant code: 

```go
// Uncomment this block to pass the first stage
"net"
"os"
```

```go
// Uncomment this block to pass the first stage

l, err := net.Listen("tcp", "0.0.0.0:6379")
if err != nil {
	fmt.Println("Failed to bind to port 6379")
	os.Exit(1)
}
_, err = l.Accept()
if err != nil {
	fmt.Println("Error accepting connection: ", err.Error())
	os.Exit(1)
}
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
