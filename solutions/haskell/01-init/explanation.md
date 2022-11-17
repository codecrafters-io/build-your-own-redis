The entry point for your Redis implementation is in `app/Main.hs`.

Study and uncomment the relevant code: 

```haskell
-- Uncomment this block to pass stage 1
let port = "6379"
putStrLn $ "\r\n>>> Redis server listening on port " ++ port ++ " <<<"
serve HostAny port $ \(socket, _address) -> do
    putStrLn $ "successfully connected client: " ++ show _address
    closeSock socket
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
