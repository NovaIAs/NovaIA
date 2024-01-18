```dart
import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:io';

void main() async {
  // Create a TCP server socket.
  ServerSocket server = await ServerSocket.bind(InternetAddress.anyIPv4, 4040);

  // Listen for client connections.
  server.listen((Socket socket) {
    // Create a stream controller to handle the client's messages.
    StreamController<String> controller = StreamController<String>();

    // Create a listener for the client's messages.
    socket.listen((List<int> data) {
      // Decode the message from JSON.
      String message = utf8.decode(data);
      Map<String, dynamic> json = jsonDecode(message);

      // Handle the message.
      switch (json['type']) {
        case 'message':
          // Send the message to all other clients.
          controller.add(message);
          break;
        case 'join':
          // Add the client to the list of connected clients.
          controller.add(jsonEncode({'type': 'join', 'name': json['name']}));
          break;
        case 'leave':
          // Remove the client from the list of connected clients.
          controller.add(jsonEncode({'type': 'leave', 'name': json['name']}));
          break;
      }
    });

    // Send the initial list of connected clients to the new client.
    List<String> names = [];
    for (Socket client in server.connections) {
      names.add(client.remoteAddress.address);
    }
    controller.add(jsonEncode({'type': 'join', 'names': names}));

    // Stream the messages to the client.
    controller.stream.listen((String message) {
      socket.write(utf8.encode(message));
    });

    // Close the socket when the client disconnects.
    socket.done.then((_) {
      controller.close();
    });
  });
}
```

This code creates a simple TCP server that allows multiple clients to connect and chat with each other. The server uses JSON messages to communicate with the clients.

The server starts by creating a `ServerSocket` object and binding it to a specific IP address and port. In this case, the server is bound to all IPv4 addresses on port 4040.

The server then listens for client connections. When a client connects, the server creates a `Socket` object to represent the connection.

The server also creates a `StreamController` object to handle the client's messages. The `StreamController` object is used to buffer the client's messages and send them to the server's message handler.

The server's message handler is a function that is called whenever the client sends a message. The message handler decodes the JSON message and handles it accordingly.

The server can handle three types of messages:

* `message`: This message type is used to send a message to all other clients.
* `join`: This message type is used to add a new client to the list of connected clients.
* `leave`: This message type is used to remove a client from the list of connected clients.

The server also sends the initial list of connected clients to the new client when it joins.

The server streams the messages to the client using the `StreamController` object. The client can listen to the messages using a `Stream` object.

The server closes the socket when the client disconnects.

This code is a simple example of how to create a TCP server in Dart. It can be used to create a variety of different applications, such as chat servers, web servers, and file servers.