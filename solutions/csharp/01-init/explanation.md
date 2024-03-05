The entry point for your Redis implementation is in `src/Server.cs`.

Study and uncomment the relevant code: 

```csharp
// Uncomment this block to pass the first stage
TcpListener server = new TcpListener(IPAddress.Any, 6379);
// Since the tester restarts your program quite often, setting SO_REUSEADDR
// ensures that we don't run into 'Address already in use' errors
server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true);
server.Start();
server.AcceptSocket(); // wait for client
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
