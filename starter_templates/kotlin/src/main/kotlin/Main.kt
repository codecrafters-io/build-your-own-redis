import java.io.IOException
import java.net.ServerSocket
import java.net.Socket

fun main(args: Array<String>) {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    println("Logs from your program will appear here!")

    // Uncomment this block to pass the first stage
    // val serverSocket: ServerSocket
    // var clientSocket: Socket? = null
    // val port = 6379
    // try {
    //     serverSocket = ServerSocket(port)
    //     serverSocket.reuseAddress = true
    //     // Wait for connection from client.
    //     clientSocket = serverSocket.accept()
    // } catch (e: IOException) {
    //     println("IOException: " + e.message)
    // } finally {
    //     try {
    //         if (clientSocket != null) {
    //             clientSocket.close()
    //         }
    //     } catch (e: IOException) {
    //         println("IOException: " + e.message)
    //     }
    // }
}
