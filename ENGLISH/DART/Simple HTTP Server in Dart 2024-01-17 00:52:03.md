```dart
// Import the necessary libraries.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Define the main function.
void main() async {
  // Create a new HTTP server.
  var server = await HttpServer.bind('127.0.0.1', 8080);

  // Handle HTTP requests.
  server.listen((request) async {
    // Get the request body.
    var body = await request.transform(utf8.decoder).join();

    // Parse the request body as JSON.
    var data = json.decode(body);

    // Do something with the data.
    print(data);

    // Send a response to the client.
    request.response.write('Hello, world!');
    request.response.close();
  });
}
```

**Explanation:**

* The `import` statements at the beginning of the code import the necessary libraries.
* The `main` function is the entry point of the program.
* The `await HttpServer.bind('127.0.0.1', 8080);` line creates a new HTTP server and binds it to the IP address `127.0.0.1` and port `8080`.
* The `server.listen((request) async {...});` line listens for HTTP requests on the server.
* The `await request.transform(utf8.decoder).join();` line gets the request body and decodes it as UTF-8.
* The `var data = json.decode(body);` line parses the request body as JSON.
* The `print(data);` line prints the data to the console.
* The `request.response.write('Hello, world!');` line sends a response to the client.
* The `request.response.close();` line closes the response.