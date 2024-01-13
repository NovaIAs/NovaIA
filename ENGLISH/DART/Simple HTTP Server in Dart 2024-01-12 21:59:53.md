```dart
// Import the necessary libraries.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Define the main function.
void main() async {
  // Create a server socket.
  ServerSocket serverSocket = await ServerSocket.bind(InternetAddress.anyIPv4, 8080);

  // Listen for incoming connections.
  serverSocket.listen((Socket socket) async {
    // Read the HTTP request from the socket.
    HttpRequest request = await HttpRequest.receiveFromSocket(socket);

    // Parse the HTTP request.
    String path = request.uri.path;
    String method = request.method;

    // Handle the HTTP request.
    if (method == 'GET' && path == '/') {
      // Send a response to the client.
      HttpResponse response = request.response;
      response.statusCode = 200;
      response.headers.add('Content-Type', 'text/plain');
      response.write('Hello, world!');
      response.close();
    } else {
      // Send a 404 error response to the client.
      HttpResponse response = request.response;
      response.statusCode = 404;
      response.headers.add('Content-Type', 'text/plain');
      response.write('Not found');
      response.close();
    }

    // Close the socket.
    socket.close();
  });
}
```

**Explanation:**

This code creates a simple HTTP server in Dart. The server listens for incoming connections on port 8080. When a client connects to the server, the server reads the HTTP request from the client and parses it. The server then handles the HTTP request and sends a response back to the client. If the client requests the root URL ('/'), the server sends a response with the message "Hello, world!". If the client requests any other URL, the server sends a 404 error response.

Here is a more detailed explanation of the code:

* The `import` statements at the beginning of the code import the necessary libraries.
* The `main` function is the entry point for the program.
* The `ServerSocket.bind` function creates a server socket and binds it to the specified IP address and port.
* The `listen` method on the server socket listens for incoming connections.
* When a client connects to the server, the `HttpRequest.receiveFromSocket` function is used to read the HTTP request from the client.
* The `uri` property of the HTTP request contains the path and query string of the requested URL.
* The `method` property of the HTTP request contains the HTTP method (e.g., GET, POST, PUT, DELETE).
* The `HttpResponse.statusCode` property sets the status code of the HTTP response.
* The `HttpResponse.headers` property adds headers to the HTTP response.
* The `HttpResponse.write` method writes data to the HTTP response.
* The `HttpResponse.close` method closes the HTTP response.
* The `socket.close` method closes the socket.