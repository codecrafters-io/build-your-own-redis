def main():
    import socket
    s = socket.create_server(("localhost", 6379), reuse_port=True)
    s.accept() # wait for client


if __name__ == "__main__":
    main()
