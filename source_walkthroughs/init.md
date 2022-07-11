In the official Redis implementation, the function responsible for binding to a port is [`listenToPort`][function-listenToPort]: 

[function-listenToPort]: https://github.com/redis/redis/blob/8203461120bf244e5c0576222c6aa5d986587bca/src/server.c#L2282


^^ referenced_code
link:https://github.com/redis/redis/blob/8203461120bf244e5c0576222c6aa5d986587bca/src/server.c#L2282-L2322
```c
/* Initialize a set of file descriptors to listen to the specified 'port'
 * binding the addresses specified in the Redis server configuration.
 * 
 * ...
 *
 */
int listenToPort(int port, socketFds *sfd) {
    // ... 
    
    if (strchr(addr,':')) {
        /* Bind IPv6 address. */
        sfd->fd[sfd->count] = anetTcp6Server(server.neterr,port,addr,server.tcp_backlog);
    } else {
        /* Bind IPv4 address. */
        sfd->fd[sfd->count] = anetTcpServer(server.neterr,port,addr,server.tcp_backlog);
    }
    
    // ...
}
```

This function is called inside the [`initServer`][function-initServer] routine which runs when a Redis server boots. 

The underlying functions that it calls, [`anetTcpServer`][function-anetTcpServer] and 
[`anetTcp6Server`][function-anetTcp6Server], use [`listen`][unix-listen] from `sys/socket.h`.

[unix-listen]: https://man7.org/linux/man-pages/man2/listen.2.html
[function-anetTcp6Server]: https://github.com/redis/redis/blob/ef68deb3c2a4d6205ddc84141d4d84b6e53cbc1b/src/anet.c#L476
[function-anetTcpServer]: https://github.com/redis/redis/blob/ef68deb3c2a4d6205ddc84141d4d84b6e53cbc1b/src/anet.c#L481
[function-initServer]: https://github.com/redis/redis/blob/8203461120bf244e5c0576222c6aa5d986587bca/src/server.c#L2391

### Updating the port on a running Redis instance

Fun fact: you can update the port on a running Redis server without a restart!

The function that handles this is `changeListenPort`: 

^^ referenced_code
link:https://github.com/redis/redis/blob/ef68deb3c2a4d6205ddc84141d4d84b6e53cbc1b/src/server.c#L6260-L6290
```c
int changeListenPort(int port, socketFds *sfd, aeFileProc *accept_handler) {
    // ...
    
    /* Close old servers */
    closeSocketListeners(sfd);

    /* Bind to the new port */
    if (listenToPort(port, &new_sfd) != C_OK) {
        return C_ERR;
    }
    
    // ...
}
```

This can be invoked by running the [`CONFIG SET`][redis-config-set-command] command like this: `CONFIG SET port <new_port>`.

[redis-config-set-command]: https://redis.io/commands/config-set
