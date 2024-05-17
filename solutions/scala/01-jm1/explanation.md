The entry point for your Redis implementation is in `src/main/scala/codecrafters_redis/Server.scala`.

Study and uncomment the relevant code: 

```scala
// Uncomment this to pass the first stage

val serverSocket = new ServerSocket()
serverSocket.bind(new InetSocketAddress("localhost", 6379))
val clientSocket = serverSocket.accept() // wait for client
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
