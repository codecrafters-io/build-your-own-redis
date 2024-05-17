The entry point for your Redis implementation is in `app/server.rb`.

Study and uncomment the relevant code: 

```ruby
# Uncomment this block to pass the first stage
server = TCPServer.new(@port)
client = server.accept
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
