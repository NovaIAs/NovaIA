```dart
// Import the necessary libraries.
import 'dart:io';
import 'dart:async';
import 'dart:convert';

// Define the main function.
void main() async {
  // Create a server socket.
  ServerSocket serverSocket = await ServerSocket.bind(InternetAddress.anyIPv4, 8080);

  // Listen for incoming connections.
  serverSocket.listen((Socket socket) async {
    // Read the request from the client.
    HttpRequest request = await HttpRequest.receiveFromSocket(socket);

    // Parse the request.
    var requestLines = request.uri.path.split('/');

    // Get the requested resource.
    var resource = requestLines.last;

    // Send the response to the client.
    if (resource == 'hello.txt') {
      // Send the contents of the hello.txt file.
      var file = File('hello.txt');
      var contents = await file.readAsString();
      var response = HttpResponse();
      response.statusCode = HttpStatus.ok;
      response.headers.add('Content-Type', 'text/plain');
      response.write(contents);
      await response.close();
    } else if (resource == 'json.json') {
      // Send a JSON object.
      var response = HttpResponse();
      response.statusCode = HttpStatus.ok;
      response.headers.add('Content-Type', 'application/json');
      response.write(jsonEncode({'message': 'Hello, world!'}));
      await response.close();
    } else {
      // Send a 404 Not Found response.
      var response = HttpResponse();
      response.statusCode = HttpStatus.notFound;
      response.headers.add('Content-Type', 'text/plain');
      response.write('404 Not Found');
      await response.close();
    }

    // Close the socket.
    socket.close();
  });
}
```

This code creates a simple HTTP server that listens for incoming connections on port 8080. When a client connects to the server, the server reads the request from the client and parses it. The server then gets the requested resource and sends it to the client. If the requested resource is not found, the server sends a 404 Not Found response.

The code is complex because it uses a number of different libraries and concepts. It also uses asynchronous programming, which can be difficult to understand. However, the code is well-commented and easy to follow.

Here is a more detailed explanation of the code:

* The `import` statements at the beginning of the file import the necessary libraries.
* The `main()` function is the entry point for the program.
* The `ServerSocket.bind()` method creates a server socket and binds it to the specified IP address and port.
* The `listen()` method on the server socket listens for incoming connections.
* When a client connects to the server, the server socket creates a new `Socket` object.
* The `HttpRequest.receiveFromSocket()` method reads the request from the client.
* The `uri.path` property of the request object contains the requested resource.
* The code uses the `split()` method to split the request path into a list of strings.
* The last element of the list is the requested resource.
* The code uses a series of `if` statements to determine how to respond to the request.
* If the requested resource is `hello.txt`, the code reads the contents of the file and sends them to the client.
* If the requested resource is `json.json`, the code creates a JSON object and sends it to the client.
* If the requested resource is not found, the code sends a 404 Not Found response.
* The `close()` method on the socket closes the connection to the client.