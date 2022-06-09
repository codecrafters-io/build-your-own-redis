The entry point for your Redis implementation is in `Main.java`.

Study and uncomment the relevant code: 

```java
//  Uncomment this block to pass the first stage
   ServerSocket serverSocket = null;
   Socket clientSocket = null;
   int port = 6379;
   try {
     serverSocket = new ServerSocket(port);
     serverSocket.setReuseAddress(true);
     // Wait for connection from client.
     clientSocket = serverSocket.accept();
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
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
