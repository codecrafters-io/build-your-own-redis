import threading
import socket

from .resp_decoder import RESPDecoder


def handle_connection(client_connection):
    while True:
        try:
            command, *args = RESPDecoder(client_connection).decode()

            if command == b"ping":
                client_connection.send(b"+PONG\r\n")
            elif command == b"echo":
                client_connection.send(b"$%d\r\n%b\r\n" % (len(args[0]), args[0]))
            else:
                client_connection.send(b"-ERR unknown command\r\n")
        except ConnectionError:
            break  # Stop serving if the client connection is closed


def main():
    server_socket = socket.create_server(("localhost", 6379), reuse_port=True)

    while True:
        client_connection, _ = server_socket.accept()  # wait for client
        threading.Thread(target=handle_connection, args=(client_connection,)).start()


if __name__ == "__main__":
    main()
