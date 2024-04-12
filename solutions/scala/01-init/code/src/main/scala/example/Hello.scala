package example

import java.net.{InetSocketAddress, ServerSocket}

object Hello {
  def main(args: Array[String]): Unit = {
    val serverSocket = new ServerSocket()
    serverSocket.bind(new InetSocketAddress("localhost", 6379))
    val clientSocket = serverSocket.accept() // wait for client
  }
}
