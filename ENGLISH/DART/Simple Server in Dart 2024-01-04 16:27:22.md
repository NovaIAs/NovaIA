```dart
// Import the necessary libraries.
import 'dart:async';
import 'dart:io';

// Define a class to represent a server.
class Server {
  // The port on which the server will listen.
  final int port;

  // A list of clients connected to the server.
  final List<Socket> clients = [];

  // Create a new server on the specified port.
  Server(this.port);

  // Start the server.
  void start() async {
    // Create a server socket and listen for connections.
    ServerSocket serverSocket = await ServerSocket.bind(InternetAddress.anyIPv4, port);

    // Accept connections from clients.
    serverSocket.listen((Socket socket) {
      // Add the client to the list of clients.
      clients.add(socket);

      // Handle data received from the client.
      socket.listen((data) {
        // Echo the data back to the client.
        socket.write(data);
      });

      // Handle the client disconnecting.
      socket.done.then((_) {
        // Remove the client from the list of clients.
        clients.remove(socket);
      });
    });
  }
}

// Create a new server on port 8080.
Server server = Server(8080);

// Start the server.
server.start();
```

This code creates a simple server that listens for connections on a specified port. When a client connects to the server, the server adds the client to a list of clients and starts listening for data from the client. When the client sends data to the server, the server echoes the data back to the client. When the client disconnects, the server removes the client from the list of clients.

The code is complex because it uses a number of different concepts, including:

* **Sockets:** A socket is a communication endpoint between two processes. In this case, the server creates a server socket to listen for connections from clients. When a client connects to the server, the server creates a client socket to communicate with the client.
* **Asynchronous programming:** Asynchronous programming allows a program to perform multiple tasks at the same time. In this case, the server uses asynchronous programming to listen for connections from clients and to handle data received from clients.
* **Event-driven programming:** Event-driven programming allows a program to respond to events, such as a client connecting to the server or a client sending data to the server. In this case, the server uses event-driven programming to handle connections from clients and to handle data received from clients.

The code is also complex because it uses a number of different libraries, including:

* **dart:async:** The dart:async library provides support for asynchronous programming.
* **dart:io:** The dart:io library provides support for input and output operations, such as creating sockets and listening for connections.
* **dart:collection:** The dart:collection library provides support for collections, such as lists and maps.

Overall, this code is a complex example of how to use sockets, asynchronous programming, and event-driven programming to create a simple server.