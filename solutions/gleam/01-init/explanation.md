The entry point for your Redis implementation is in `src/redis.gleam`.

Study and uncomment the relevant code: 

```gleam
// Uncomment this block to pass the first stage
import gleam/result
import glisten/socket/options.{ActiveMode, Passive}
import glisten/tcp
```

```gleam
// Uncomment this block to pass the first stage
use listener <- result.then(tcp.listen(6379, [ActiveMode(Passive)]))
use _socket <- result.then(tcp.accept(listener))
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
