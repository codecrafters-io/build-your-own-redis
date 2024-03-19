The entry point for your Redis implementation is in `app/Sources/RedisServer.swift`.

Study and uncomment the relevant code: 

```swift
// Uncomment this block to pass the first stage

    let channel = try serverBootstrap.bind(host: defaultHost, port: defaultPort).wait()
    print("Server started and listening on \(channel.localAddress!)")
    try channel.closeFuture.wait()
    print("Server closed")
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
