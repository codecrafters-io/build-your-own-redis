# ConnectionBuffer wraps socket.Socket and adds support for reading until a delimiter.
class ConnectionBuffer:
    def __init__(self, connection):
        self.connection = connection
        self.buffer = b''

    def read_until_delimiter(self, delimiter):
        while delimiter not in self.buffer:
            data = self.connection.recv(1024)

            if not data:  # socket closed
                return None

            self.buffer += data

        data_before_delimiter, delimiter, self.buffer = self.buffer.partition(delimiter)
        return data_before_delimiter

    def read(self, bufsize):
        if len(self.buffer) < bufsize:
            data = self.connection.recv(1024)

            if not data:  # socket closed
                return None

            self.buffer += data

        data, self.buffer = self.buffer[:bufsize], self.buffer[bufsize:]
        return data


class RESPDecoder:
    def __init__(self, connection):
        self.connection = ConnectionBuffer(connection)

    def decode(self):
        data_type_byte = self.connection.read(1)

        if data_type_byte == b"+":
            return self.decode_simple_string()
        elif data_type_byte == b"$":
            return self.decode_bulk_string()
        elif data_type_byte == b"*":
            return self.decode_array()
        else:
            raise Exception(f"Unknown data type byte: {data_type_byte}")

    def decode_simple_string(self):
        return self.connection.read_until_delimiter(b"\r\n")

    def decode_bulk_string(self):
        bulk_string_length = int(self.connection.read_until_delimiter(b"\r\n"))
        data = self.connection.read(bulk_string_length)
        assert self.connection.read_until_delimiter(b"\r\n") == b""  # delimiter should be immediately after string
        return data

    def decode_array(self):
        result = []
        array_length = int(self.connection.read_until_delimiter(b"\r\n"))

        for _ in range(array_length):
            result.append(self.decode())

        return result
