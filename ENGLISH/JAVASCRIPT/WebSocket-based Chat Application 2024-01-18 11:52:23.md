```javascript
// Setup a WebSocket connection to a server
const socket = new WebSocket('ws://echo.websocket.org');

// Define an object to store the state of the application
const state = {
  messages: [],
  users: [],
  currentUser: null,
};

// Define an object to store the UI elements
const ui = {
  messageInput: document.getElementById('message-input'),
  messageList: document.getElementById('message-list'),
  userList: document.getElementById('user-list'),
};

// Define a function to handle incoming messages from the server
const handleServerMessage = (event) => {
  const data = JSON.parse(event.data);
  switch (data.type) {
    // Handle a message that contains a new message
    case 'message':
      state.messages.push(data.message);
      updateMessageList();
      break;
    // Handle a message that contains a list of users
    case 'users':
      state.users = data.users;
      updateUserList();
      break;
    // Handle a message that contains the current user's information
    case 'currentUser':
      state.currentUser = data.user;
      break;
  }
};

// Define a function to send a message to the server
const sendMessage = (message) => {
  socket.send(JSON.stringify({
    type: 'message',
    message: message,
  }));
};

// Define a function to update the list of messages in the UI
const updateMessageList = () => {
  ui.messageList.innerHTML = '';
  state.messages.forEach((message) => {
    const li = document.createElement('li');
    li.textContent = message;
    ui.messageList.appendChild(li);
  });
};

// Define a function to update the list of users in the UI
const updateUserList = () => {
  ui.userList.innerHTML = '';
  state.users.forEach((user) => {
    const li = document.createElement('li');
    li.textContent = user.username;
    ui.userList.appendChild(li);
  });
};

// Add an event listener to listen for incoming messages from the server
socket.addEventListener('message', handleServerMessage);

// Add an event listener to listen for when the user presses the send button
ui.messageInput.addEventListener('keypress', (event) => {
  if (event.key === 'Enter') {
    sendMessage(ui.messageInput.value);
    ui.messageInput.value = '';
  }
});
```

Explanation:

This code sets up a WebSocket connection to a server, defines a state object to store the application state, and defines UI elements. It includes functions to handle incoming messages from the server, send messages to the server, update the list of messages and users in the UI, and listen for user input. Overall, it creates a simple chat application that allows users to send and receive messages and see a list of users connected to the server.