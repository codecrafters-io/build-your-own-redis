<?php
error_reporting(E_ALL);

$server = stream_socket_server("tcp://0.0.0.0:6379", $errno, $errstr);
if (!$server) {
    fwrite(STDERR, "Failed to bind to port 6379\n");
    exit(1);
}
$client = stream_socket_accept($server, -1); // wait for client