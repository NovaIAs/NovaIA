```dart
import 'dart:io';

void main() {
  // Create a server socket
  ServerSocket.bind(InternetAddress.loopbackIPv4, 8080).then((serverSocket) {
    // Listen for incoming connections
    serverSocket.listen((socket) {
      // Handle the connection
      _handleConnection(socket);
    });
  });
}

void _handleConnection(Socket socket) {
  // Read the request from the client
  socket.listen((data) {
    // Parse the request
    HttpRequest request = HttpRequest.fromBytes(data);

    // Handle the request
    _handleRequest(request, socket);
  });
}

void _handleRequest(HttpRequest request, Socket socket) {
  // Get the resource path from the request
  String resourcePath = request.uri.path;

  // Check if the resource exists
  File file = new File(resourcePath);
  if (!file.existsSync()) {
    // Send a 404 Not Found response
    _sendNotFoundResponse(socket);
    return;
  }

  // Read the file
  file.openRead().then((fileStream) {
    // Send the file contents to the client
    socket.addStream(fileStream);

    // Close the connection
    socket.close();
  });
}

void _sendNotFoundResponse(Socket socket) {
  // Create a 404 Not Found response
  HttpResponse response = new HttpResponse();
  response.statusCode = HttpStatus.notFound;
  response.reasonPhrase = 'Not Found';
  response.headers.add('Content-Type', 'text/plain');

  // Send the response to the client
  socket.add(response.toString().codeUnits);

  // Close the connection
  socket.close();
}
```

Explanation:

1. `ServerSocket.bind()`: This line creates a server socket on port 8080. The server socket listens for incoming connections.
2. `serverSocket.listen()`: This line listens for incoming connections on the server socket. When a client connects, the `_handleConnection()` method is called.
3. `_handleConnection()`: This method handles the incoming connection. It reads the request from the client and calls `_handleRequest()` to handle the request.
4. `_handleRequest()`: This method handles the request from the client. It gets the resource path from the request and checks if the resource exists. If the resource exists, it reads the file and sends the file contents to the client. If the resource does not exist, it sends a 404 Not Found response.
5. `_sendNotFoundResponse()`: This method creates a 404 Not Found response and sends it to the client.