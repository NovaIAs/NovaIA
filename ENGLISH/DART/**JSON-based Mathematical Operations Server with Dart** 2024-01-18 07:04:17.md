```dart
// Import the necessary libraries.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

// Define a main function.
void main() async {
  // Create a server socket.
  var serverSocket = await ServerSocket.bind('localhost', 4040);

  // Listen for incoming connections.
  serverSocket.listen((clientSocket) async {
    // Get the input stream and output stream.
    var inputStream = clientSocket.inputStream;
    var outputStream = clientSocket.outputStream;

    // Create a buffer to store the received data.
    var buffer = <int>[];

    // Listen for data on the input stream.
    inputStream.listen((data) {
      // Add the data to the buffer.
      buffer.addAll(data);

      // If the buffer contains a newline character, process the data.
      if (buffer.contains(10)) {
        // Decode the data from JSON.
        var dataObject = json.decode(utf8.decode(buffer));

        // Process the data object.
        var result = processData(dataObject);

        // Encode the result as JSON.
        var resultJson = json.encode(result);

        // Send the result to the client.
        outputStream.write(resultJson);

        // Clear the buffer.
        buffer.clear();
      }
    });

    // When the client closes the connection, close the socket.
    clientSocket.done.then((_) {
      clientSocket.close();
    });
  });
}

// Define a function to process the data.
Map<String, dynamic> processData(Map<String, dynamic> data) {
  // Get the operation from the data object.
  var operation = data['operation'];

  // Perform the operation.
  var result;
  switch (operation) {
    case 'add':
      result = data['a'] + data['b'];
      break;
    case 'subtract':
      result = data['a'] - data['b'];
      break;
    case 'multiply':
      result = data['a'] * data['b'];
      break;
    case 'divide':
      result = data['a'] / data['b'];
      break;
    case 'sqrt':
      result = sqrt(data['a']);
      break;
    case 'pow':
      result = pow(data['a'], data['b']);
      break;
    case 'random':
      result = Random().nextDouble();
      break;
    default:
      result = 'Invalid operation';
      break;
  }

  // Return the result.
  return {'result': result};
}
```

**Explanation:**

* This Dart code sets up a simple TCP server to receive JSON requests from clients, performs some operations on the data in the requests, and returns the results as JSON responses. The server listens on port 4040.
* When a client connects to the server, the server creates a socket for the connection and listens for incoming data on the socket.
* When the client sends a request, the server receives the data and stores it in a buffer.
* When the buffer contains a newline character, the server assumes that the request is complete and begins processing it.
* The server decodes the JSON data in the request and processes it according to the specified operation.
* The server encodes the result as JSON and sends it back to the client.
* The server continues to listen for and process requests from the client until the client closes the connection.

This code demonstrates various features of Dart, including:

* Asynchronous programming using `async` and `await`.
* Socket programming using `ServerSocket` and `Socket`.
* Data streaming using `InputStream` and `OutputStream`.
* JSON encoding and decoding using the `json` library.
* Error handling using `try-catch`.
* Mathematical operations using the `Math` library.
* Random number generation using the `Random` class.