The entry point for your Redis implementation is in `src/redis.gleam`.

Study and uncomment the relevant code: 

```gleam
// Uncomment this block to pass the first stage
use _listener <- result.then(tcp.listen(8000, [ActiveMode(Passive)]))
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
