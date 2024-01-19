```dart
import 'dart:io';
import 'dart:async';
import 'dart:convert';

// A simple server that serves a JSON response to requests.
void main() async {
  // Create a server that listens on port 8080.
  var server = await HttpServer.bind(InternetAddress.loopbackIPv4, 8080);

  // Handle requests.
  server.listen((request) async {
    // Parse the request body as JSON.
    var body = await utf8.decodeStream(request.inputStream);
    var data = json.decode(body);

    // Check if the request is valid.
    if (!data.containsKey('name')) {
      request.response.statusCode = 400;
      request.response.write('Missing parameter: name');
      return;
    }

    // Generate a response.
    var response = {
      'message': 'Hello, ${data['name']}!'
    };

    // Send the response.
    request.response.statusCode = 200;
    request.response.headers.contentType = ContentType.json;
    request.response.write(json.encode(response));
    request.response.close();
  });
}
```

**Explanation:**

This code creates a simple server that serves a JSON response to requests. The server listens on port 8080 and handles requests by parsing the request body as JSON, checking if the request is valid, generating a response, and sending the response. The response is a JSON object with a message property. The message property contains a greeting message that includes the name of the person specified in the request body.

Here is a breakdown of the code:

* The `import` statements import the necessary libraries.
* The `main` function is the entry point of the program.
* The `await HttpServer.bind` method creates a server that listens on port 8080.
* The `server.listen` method listens for requests on the server.
* The `await utf8.decodeStream(request.inputStream)` method parses the request body as a string.
* The `json.decode` method parses the request body as JSON.
* The `if (!data.containsKey('name'))` statement checks if the request body contains a `name` property.
* The `request.response.statusCode` property sets the status code of the response.
* The `request.response.write` method writes a message to the response.
* The `request.response.close()` method closes the response.
* The `var response = {...}` statement generates a response object.
* The `request.response.headers.contentType` property sets the content type of the response to JSON.
* The `json.encode` method converts the response object to JSON.
* The `request.response.write` method writes the JSON response to the response.
* The `request.response.close()` method closes the response.

This code is a good example of how to create a simple server in Dart. It can be used as a starting point for more complex applications.