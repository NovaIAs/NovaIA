```
// Import necessary libraries
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Define the main function
void main() async {
  // Create an HTTP server
  HttpServer server = await HttpServer.bind('127.0.0.1', 8080);

  // Handle HTTP requests
  server.listen((HttpRequest request) async {
    // Get the request body
    String body = await request.transform(utf8.decoder).join();

    // Parse the request body as JSON
    Map<String, dynamic> data = json.decode(body);

    // Process the request
    String responseBody = 'Hello, ${data['name']}!';

    // Send the response
    request.response.write(responseBody);
    request.response.close();
  });

  // Print a message to the console
  print('Server listening on port 8080');
}
```

Explanation:

1. `import` Statements:
   - We start by importing the necessary libraries:
     - `'dart:async'`: For asynchronous programming.
     - `'dart:convert'`: For JSON parsing.
     - `'dart:io'`: For HTTP server functionality.

2. `main()` Function:
   - This is the entry point of the program.

3. Creating an HTTP Server:
   - We create an HTTP server using `HttpServer.bind()` and specify the IP address ('127.0.0.1') and port (8080).

4. Handling HTTP Requests:
   - We use the `listen()` method of the server to handle HTTP requests.

5. Getting the Request Body:
   - We extract the request body, which contains the JSON data sent by the client, using `request.transform(utf8.decoder).join()`.

6. Parsing JSON Data:
   - We parse the request body as JSON using `json.decode()`.

7. Processing the Request:
   - In this example, we simply create a response message based on the `name` field in the JSON data.

8. Sending the Response:
   - We use `request.response.write()` to write the response body to the client.
   - We then call `request.response.close()` to send the response.

9. Console Message:
   - We print a message to the console indicating that the server is listening on port 8080.

This code demonstrates how to create an HTTP server in Dart, handle HTTP requests, parse JSON data in the request body, process the request, and send a response.