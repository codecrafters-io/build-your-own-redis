The entry point for your Redis implementation is in `src/main.cpp`.

Study and uncomment the relevant code: 

```cpp
// Uncomment this block to pass the first stage

accept(server_fd, (struct sockaddr *) &client_addr, (socklen_t *) &client_addr_len);
std::cout << "Client connected\n";

close(server_fd);
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
