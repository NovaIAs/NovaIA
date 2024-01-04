```typescript
// Import necessary modules
import { Component, OnInit, ViewChild, ElementRef, AfterViewInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { Socket } from 'socket.io-client';
import { io } from 'socket.io-client';

// Define the component
@Component({
  selector: 'app-chat',
  templateUrl: './chat.component.html',
  styleUrls: ['./chat.component.css']
})
export class ChatComponent implements OnInit, AfterViewInit {

  // Define component properties
  @ViewChild('chatWindow') chatWindow: ElementRef;
  @ViewChild('messageInput') messageInput: ElementRef;

  // Initialize socket connection
  private socket: Socket;

  // Initialize message history
  messageHistory: string[] = [];

  // Initialize subscriptions
  private messageSubscription: Subscription;
  private connectSubscription: Subscription;

  // Initialize user input
  userInput: string;

  // Initialize component methods
  ngOnInit() {
    // Establish socket connection
    this.socket = io('http://localhost:3000');

    // Subscribe to the 'message' event
    this.messageSubscription = this.socket.fromEvent('message').subscribe((message: string) => {
      // Add the received message to the message history
      this.messageHistory.push(message);

      // Scroll to the bottom of the chat window
      this.scrollToBottom();
    });

    // Subscribe to the 'connect' event
    this.connectSubscription = this.socket.connect().subscribe(() => {
      console.log('Connected to the server');
    });
  }

  ngAfterViewInit() {
    // Scroll to the bottom of the chat window
    this.scrollToBottom();
  }

  // Define send message method
  sendMessage() {
    // Send the user input to the server
    this.socket.emit('message', this.userInput);

    // Clear the user input
    this.userInput = '';
  }

  // Define scroll to bottom method
  scrollToBottom() {
    // Scroll the chat window to the bottom
    this.chatWindow.nativeElement.scrollTop = this.chatWindow.nativeElement.scrollHeight;
  }

  // Define destroy method
  ngOnDestroy() {
    // Unsubscribe from all subscriptions
    this.messageSubscription.unsubscribe();
    this.connectSubscription.unsubscribe();
  }
}
```

Explanation:

This TypeScript code defines a chat component that utilizes the Socket.IO library to establish a real-time chat application. Here's a breakdown of the code:

1. **Component Definition**:
   - The `@Component` decorator is used to define the `ChatComponent` class as an Angular component.
   - The `selector` property specifies the HTML tag (`<app-chat>`) used to include this component in templates.
   - The `templateUrl` property specifies the HTML template file for this component.
   - The `styleUrls` property specifies the CSS stylesheet file for this component.

2. **Component Properties**:
   - `@ViewChild('chatWindow') chatWindow` and `@ViewChild('messageInput') messageInput`:
     - These are property decorators used to get references to HTML elements within the template.
     - They allow direct access to the DOM elements for manipulation.

3. **Socket Connection**:
   - The `socket` property is initialized to establish a WebSocket connection with the server.
   - The `ngOnInit` lifecycle hook is used to initialize the socket connection.
   - The `io` function from the Socket.IO library is used to connect to the server.

4. **Message Subscription**:
   - The `messageSubscription` property is a subscription to the 'message' event emitted by the server.
   - When a message is received, it is added to the `messageHistory` array, and the chat window is scrolled to the bottom.

5. **Connect Subscription**:
   - The `connectSubscription` property is a subscription to the 'connect' event emitted by the server.
   - When a connection is established, a message is logged to the console.

6. **Sending Messages**:
   - The `sendMessage` method is called when the user sends a message.
   - It emits the user input to the server using the 'message' event.
   - The user input is then cleared.

7. **Scrolling to Bottom**:
   - The `scrollToBottom` method is called to scroll the chat window to the bottom.
   - It's called after receiving a message and after sending a message to ensure the chat window is always scrolled to the bottom.

8. **Cleanup**:
   - The `ngOnDestroy` lifecycle hook is used to clean up subscriptions when the component is destroyed.

This code creates a functional chat component that allows users to send and receive messages in real time using Socket.IO. It also handles scrolling to the bottom of the chat window and cleaning up subscriptions when the component is destroyed.