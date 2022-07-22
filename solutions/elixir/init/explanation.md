The entry point for your Redis implementation is in `lib/server.ex`.

Study and uncomment the relevant code: 

```elixir
# Uncomment this block to pass the first stage

{:ok, socket} = :gen_tcp.listen(6379, [:binary, active: false, reuseaddr: true])
{:ok, _client} = :gen_tcp.accept(socket)
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
