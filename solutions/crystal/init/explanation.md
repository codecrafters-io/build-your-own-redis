The entry point for your Redis implementation is in `app/main.cr`.

Study and uncomment the relevant code: 

```crystal
# Uncomment this block to pass the first stage
server = TCPServer.new("0.0.0.0", 6379, reuse_port: true)
client = server.accept?
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
