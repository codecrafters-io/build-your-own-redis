using System.Net;
using System.Net.Sockets;

// You can use print statements as follows for debugging, they'll be visible when running tests.
Console.WriteLine("Logs from your program will appear here!");

// Uncomment this block to pass the first stage
// TcpListener server = new TcpListener(IPAddress.Any, 6379);
// // Since the tester restarts your program quite often, setting SO_REUSEADDR
// // ensures that we don't run into 'Address already in use' errors
// server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true);
// server.Start();
// server.AcceptSocket(); // wait for client
