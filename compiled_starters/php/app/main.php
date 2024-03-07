<?php
error_reporting(E_ALL);

// You can use print statements as follows for debugging, they'll be visible when running tests.
echo "Logs from your program will appear here";

// Uncomment this to pass the first stage
// $sock = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
// // Since the tester restarts your program quite often, setting SO_REUSEADDR
// // ensures that we don't run into 'Address already in use' errors
// socket_set_option($sock, SOL_SOCKET, SO_REUSEADDR, 1);
// socket_bind($sock, "localhost", 6379);
// socket_listen($sock, 5);
// socket_accept($sock); // Wait for first client

// socket_close($sock);
?>
