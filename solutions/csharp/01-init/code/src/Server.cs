using System.Net;
using System.Net.Sockets;

TcpListener server = new TcpListener(IPAddress.Any, 6379);
// Since the tester restarts your program quite often, setting SO_REUSEADDR
// ensures that we don't run into 'Address already in use' errors
server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true);
server.Start();
server.AcceptSocket(); // wait for client
