```typescript
// Import the necessary libraries.
import { Injectable, Inject, NgZone } from '@angular/core';
import { Observable, Subject } from 'rxjs';

// Define the service class.
@Injectable({
  providedIn: 'root'
})
export class WebSocketService {

  // Define the private variables.
  private webSocket: WebSocket;
  private subject: Subject<MessageEvent>;

  // Define the constructor.
  constructor(private zone: NgZone) { }

  // Define the method to establish a WebSocket connection.
  public connect(url: string): Observable<MessageEvent> {
    // Create a new WebSocket connection.
    this.webSocket = new WebSocket(url);

    // Create a new Subject to expose an observable of WebSocket messages.
    this.subject = new Subject<MessageEvent>();

    // Open the WebSocket connection.
    this.webSocket.onopen = (event: Event) => {
      // Notify the observer that the WebSocket connection is open.
      this.zone.run(() => {
        this.subject.next(event);
      });
    };

    // Handle WebSocket messages.
    this.webSocket.onmessage = (event: MessageEvent) => {
      // Notify the observer that a new WebSocket message has been received.
      this.zone.run(() => {
        this.subject.next(event);
      });
    };

    // Handle WebSocket errors.
    this.webSocket.onerror = (event: Event) => {
      // Notify the observer that an error has occurred.
      this.zone.run(() => {
        this.subject.error(event);
      });
    };

    // Handle WebSocket close events.
    this.webSocket.onclose = (event: CloseEvent) => {
      // Notify the observer that the WebSocket connection has been closed.
      this.zone.run(() => {
        this.subject.complete();
      });
    };

    // Return the observable of WebSocket messages.
    return this.subject.asObservable();
  }

  // Define the method to send a message via the WebSocket connection.
  public send(message: any): void {
    if (this.webSocket.readyState === WebSocket.OPEN) {
      this.webSocket.send(JSON.stringify(message));
    }
  }

  // Define the method to close the WebSocket connection.
  public close(): void {
    this.webSocket.close();
  }
}
```

**Explanation:**

This code defines a WebSocket service in TypeScript. It offers a way to establish a WebSocket connection, listen for messages, send messages, and close the connection. The service uses the WebSocket API and RxJS observables to provide a reactive programming interface for working with WebSockets.

The `connect()` method is used to establish a WebSocket connection. It takes a URL as an argument and returns an Observable of WebSocket messages. The `send()` method is used to send a message via the WebSocket connection. The `close()` method is used to close the WebSocket connection.

The service uses RxJS observables to expose an observable of WebSocket messages, which allows the component that uses the service to subscribe to the observable and receive WebSocket messages as they arrive. The service also handles errors and connection close events.