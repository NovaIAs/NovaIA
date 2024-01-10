```dart
// Import the necessary libraries.
import 'dart:convert';
import 'dart:io';

// Define the main function.
void main() {
  // Create a new HTTP server.
  HttpServer server = new HttpServer();

  // Listen for requests on port 8080.
  server.listen('127.0.0.1', 8080);

  // Handle requests.
  server.onRequest.listen((HttpRequest request) {
    // Parse the request body.
    var body = JSON.decode(request.body);

    // Get the operation to perform.
    var operation = body['operation'];

    // Perform the operation.
    var result;
    switch (operation) {
      case 'add':
        result = body['a'] + body['b'];
        break;
      case 'subtract':
        result = body['a'] - body['b'];
        break;
      case 'multiply':
        result = body['a'] * body['b'];
        break;
      case 'divide':
        result = body['a'] / body['b'];
        break;
    }

    // Send the result back to the client.
    request.response.write(JSON.encode(result));
    request.response.close();
  });
}
```

This code creates a simple HTTP server that can perform basic arithmetic operations. The server listens for requests on port 8080 and handles them by parsing the request body, getting the operation to perform, performing the operation, and sending the result back to the client.

Here is a breakdown of the code:

* The `import` statements import the necessary libraries.
* The `main` function is the entry point for the program.
* The `HttpServer` class is used to create a new HTTP server.
* The `listen` method is used to listen for requests on a specific port.
* The `onRequest` event is fired when a request is received.
* The `JSON` class is used to parse the request body.
* The `operation` variable is used to store the operation to be performed.
* The `switch` statement is used to perform the operation.
* The `result` variable is used to store the result of the operation.
* The `write` method is used to send the result back to the client.
* The `close` method is used to close the request.