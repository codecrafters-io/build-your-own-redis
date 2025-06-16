package codecrafters_redis

import java.net.{InetSocketAddress, ServerSocket}

object Server {
  def main(args: Array[String]): Unit = {
    val serverSocket = new ServerSocket()
    serverSocket.bind(new InetSocketAddress("localhost", 6379))
    val clientSocket = serverSocket.accept() // wait for client
  }
}
