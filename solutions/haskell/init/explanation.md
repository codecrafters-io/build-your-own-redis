The entry point for your Redis implementation is in `app/Main.hs`.

Study and uncomment the relevant code: 

```haskell
-- Uncomment this to pass stage 1
sock <- socket AF_INET Stream defaultProtocol
setSocketOption sock ReuseAddr 1
bind sock (SockAddrInet 6379 0)
listen sock 5
_ <- accept sock
return ()
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
