import java.io.IOException;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class Main {
  public static void main(String[] args){
       ServerSocket serverSocket = null;
       Socket clientSocket = null;
       int port = 6379;
       try {
         serverSocket = new ServerSocket(port);
         // Since the tester restarts your program quite often, setting SO_REUSEADDR
         // ensures that we don't run into 'Address already in use' errors
         serverSocket.setReuseAddress(true);
         // Wait for connection from client.
         clientSocket = serverSocket.accept();
         OutputStream outputStream = clientSocket.getOutputStream();
         outputStream.write("+PONG\r\n".getBytes());
       } catch (IOException e) {
         System.out.println("IOException: " + e.getMessage());
       } finally {
         try {
           if (clientSocket != null) {
             clientSocket.close();
           }
         } catch (IOException e) {
           System.out.println("IOException: " + e.getMessage());
         }
       }
  }
}
