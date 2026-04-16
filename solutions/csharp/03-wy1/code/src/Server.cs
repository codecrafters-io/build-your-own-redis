using System.Net;
using System.Net.Sockets;
using System.Text;

TcpListener server = new TcpListener(IPAddress.Any, 6379);
server.Start();
Socket client = server.AcceptSocket();
byte[] buffer = new byte[1024];
while (true)
{
    int bytesRead = client.Receive(buffer);
    if (bytesRead == 0)
        break;
    client.Send(Encoding.UTF8.GetBytes("+PONG\r\n"));
}
