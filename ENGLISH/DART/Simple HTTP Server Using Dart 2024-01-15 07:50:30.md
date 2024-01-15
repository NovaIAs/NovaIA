```dart
import 'dart:io';
import 'dart:async';

Future<void> main() async {
  // Create a server socket
  ServerSocket serverSocket = await ServerSocket.bind(InternetAddress.anyIPv4, 8080);

  // Listen for incoming connections
  serverSocket.listen((Socket clientSocket) async {
    // Read the HTTP request from the client
    HttpRequest request = await HttpRequest.receiveFromSocket(clientSocket);

    // Parse the HTTP request
    String requestMethod = request.method;
    String requestUri = request.uri.toString();
    Map<String, String> requestHeaders = request.headers;

    // Print the HTTP request
    print('Received HTTP request:');
    print('Method: $requestMethod');
    print('URI: $requestUri');
    print('Headers:');
    requestHeaders.forEach((key, value) {
      print('  $key: $value');
    });

    // Send a HTTP response to the client
    HttpResponse response = HttpResponse(clientSocket);
    response.statusCode = 200;
    response.headers.add('Content-Type', 'text/plain');
    response.write('Hello, world!');
    await response.close();

    // Close the client socket
    clientSocket.close();
  });
}
```

This code creates a simple HTTP server that listens for incoming connections on port 8080. When a client connects to the server, the server reads the HTTP request from the client, parses the request, and sends a HTTP response to the client.

The code uses the `ServerSocket` class to create a server socket. The `ServerSocket` class represents a socket that listens for incoming connections. The `bind()` method of the `ServerSocket` class is used to bind the server socket to a specific IP address and port.

The `listen()` method of the `ServerSocket` class is used to listen for incoming connections. When a client connects to the server, the `listen()` method returns a `Socket` object that represents the connection between the server and the client.

The `HttpRequest` class is used to read the HTTP request from the client. The `HttpRequest.receiveFromSocket()` method is used to read the HTTP request from the client socket.

The `HttpRequest` class has a number of properties that can be used to access the information in the HTTP request. The `method` property contains the HTTP method (e.g., `GET`, `POST`, `PUT`, etc.). The `uri` property contains the URI of the resource that the client is requesting. The `headers` property contains a map of the HTTP headers that were sent by the client.

The `HttpResponse` class is used to send a HTTP response to the client. The `HttpResponse` class has a number of properties that can be used to set the content of the HTTP response. The `statusCode` property contains the HTTP status code (e.g., `200`, `404`, etc.). The `headers` property contains a map of the HTTP headers that will be sent to the client. The `write()` method is used to write the content of the HTTP response to the client.

The `close()` method of the `HttpResponse` class is used to close the HTTP response. The `close()` method flushes the content of the HTTP response to the client and closes the connection between the server and the client.

The `Socket` class has a `close()` method that is used to close the connection between the server and the client. The `close()` method flushes any remaining data that is in the socket and closes the socket.