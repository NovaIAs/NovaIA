```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// This code snippet creates a RESTful API using the 'shelf' package.
// It defines a simple endpoint to get a list of users.

// Import the necessary libraries.
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart';

// Define the main function.
main() async {
  // Create a list of users.
  var users = [
    {'id': 1, 'name': 'John Doe'},
    {'id': 2, 'name': 'Jane Smith'},
    {'id': 3, 'name': 'Bill Jones'}
  ];

  // Create a handler function to handle GET requests to the '/users' endpoint.
  var handler = (Request request) {
    // Check if the request method is GET.
    if (request.method == 'GET') {
      // Return a JSON response with the list of users.
      return Response.ok(jsonEncode(users));
    } else {
      // Return a 405 Method Not Allowed error response.
      return Response.methodNotAllowed();
    }
  };

  // Create a server and listen on port 8080.
  var server = await serve(handler, 'localhost', 8080);

  // Print a message to the console.
  print('Server listening on port ${server.port}');
}
```

**Explanation:**

1. **Import Statements**:
   - `import 'dart:async';`: This import statement includes the `async` library, which is necessary for asynchronous programming.
   - `import 'dart:convert';`: This import statement includes the `convert` library, which is used for encoding and decoding JSON data.
   - `import 'dart:io';`: This import statement includes the `io` library, which is used for input and output operations.
   - `import 'package:shelf/shelf.dart';`: This import statement includes the `shelf` package, which is used for creating a RESTful API.
   - `import 'package:shelf/shelf_io.dart';`: This import statement includes the `shelf_io` package, which is used for serving the API.

2. **Main Function**:
   - The `main` function is the entry point of the program.

3. **User List**:
   - A list of users is defined as a `List<Map<String, dynamic>>`. Each user object is represented by a map with two keys: `id` and `name`.

4. **Request Handler**:
   - The `handler` function is a request handler function that handles incoming HTTP requests. It takes a `Request` object as an argument and returns a `Response` object.
   - The request handler checks the request method using the `request.method` property. If the request method is `GET`, it returns a JSON response with the list of users. If the request method is not `GET`, it returns a 405 Method Not Allowed error response.

5. **Server Creation**:
   - The `serve` function is used to create a server that listens on a specified port and handles incoming requests using the provided request handler. In this case, the server is configured to listen on port 8080 and handle requests using the `handler` function.

6. **Console Output**:
   - The `print` statement is used to print a message to the console, indicating that the server is listening on the specified port.