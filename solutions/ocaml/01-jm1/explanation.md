The entry point for your Redis implementation is in `src/main.ml`.

Study and uncomment the relevant code: 

```ocaml
(* Uncomment this block to pass the first stage *)
let (client_socket, _) = accept server_socket in
close client_socket;
close server_socket
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
