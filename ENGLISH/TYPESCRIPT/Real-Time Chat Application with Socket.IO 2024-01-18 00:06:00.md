```typescript
// Client-side code
const socket = io();

// Listen for messages from the server
socket.on('message', (message) => {
  console.log(`Received message: ${message}`);
});

// Send a message to the server
const sendMessage = (message) => {
  socket.emit('message', message);
};

// Get a reference to the HTML elements
const messageInput = document.getElementById('message-input');
const sendMessageButton = document.getElementById('send-message-button');

// Add an event listener to the send message button
sendMessageButton.addEventListener('click', () => {
  const message = messageInput.value;
  sendMessage(message);
});

// Server-side code
const express = require('express');
const socketIO = require('socket.io');

// Create an Express app
const app = express();

// Serve static files from the public directory
app.use(express.static('public'));

// Start the server
const server = app.listen(3000, () => {
  console.log('Server listening on port 3000');
});

// Create a Socket.IO instance and attach it to the server
const io = socketIO(server);

// Listen for new connections
io.on('connection', (socket) => {
  console.log('New client connected');

  // Listen for messages from the client
  socket.on('message', (message) => {
    console.log(`Received message: ${message}`);

    // Broadcast the message to all other clients
    socket.broadcast.emit('message', message);
  });

  // Listen for when the client disconnects
  socket.on('disconnect', () => {
    console.log('Client disconnected');
  });
});
```

Explanation:

* The client-side code uses Socket.IO to connect to the server and send and receive messages.
* The server-side code uses Express to serve static files and Socket.IO to listen for new connections and messages from clients.
* The code allows for real-time communication between multiple clients connected to the server.

Here's a breakdown of the code:

* **Client-side code:**
    * The code uses the Socket.IO library to connect to the server.
    * It defines a function called `sendMessage` that sends a message to the server.
    * It uses the HTML DOM API to get references to the message input and send message button elements.
    * It adds an event listener to the send message button that calls the `sendMessage` function when the button is clicked.
* **Server-side code:**
    * The code uses the Express library to create an Express app and serve static files from the public directory.
    * It creates a Socket.IO instance and attaches it to the server.
    * It listens for new client connections and messages from clients.
    * It broadcasts messages received from clients to all other connected clients.

Overall, this code demonstrates how to use Socket.IO for real-time communication between a client and a server.