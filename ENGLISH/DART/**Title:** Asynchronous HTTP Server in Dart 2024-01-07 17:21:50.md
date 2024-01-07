```dart
// Import necessary libraries.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Define a function to handle the incoming HTTP requests.
Future<void> handleRequest(HttpRequest request, HttpResponse response) async {
  // Parse the request body.
  final body = await utf8.decoder.bind(request).join();

  // Process the request body.
  final data = json.decode(body);

  // Prepare the response.
  response.headers.contentType = ContentType.json;
  response.statusCode = HttpStatus.ok;

  // Send the response.
  response.write(json.encode({
    'message': 'Hello, ${data['name']}!',
  }));
}

// Create an HTTP server.
final server = await HttpServer.bind('localhost', 8080);

// Listen for incoming HTTP requests.
server.listen(handleRequest);
```

**Explanation:**

This code defines a simple HTTP server using the `dart:io` library. The server listens on port 8080 and responds to incoming HTTP requests.

When a request is received, the code parses the request body and extracts the `name` field from the JSON data. It then constructs a JSON response message and sends it back to the client.

The code uses asynchronous programming to handle the incoming HTTP requests. This allows the server to handle multiple requests concurrently without blocking.

Here is a breakdown of the code:

* The `handleRequest` function is defined to handle the incoming HTTP requests.
* The `body` variable is used to store the request body.
* The `data` variable is used to store the parsed JSON data.
* The `response` variable is used to store the HTTP response.
* The `server` variable is used to store the HTTP server.
* The `listen` method is used to listen for incoming HTTP requests.

The code is complex because it uses asynchronous programming and multiple libraries. However, it is also well-structured and easy to understand. The code is also well-commented, which makes it easier to understand what each part of the code does.