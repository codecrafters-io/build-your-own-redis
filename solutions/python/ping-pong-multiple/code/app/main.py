import socket


def main():
    server_socket = socket.create_server(("localhost", 6379), reuse_port=True)
    client_connection, _ = server_socket.accept()  # wait for client

    while True:
        client_connection.recv(1024)  # wait for client to send data
        client_connection.send(b"+PONG\r\n")


if __name__ == "__main__":
    main()
