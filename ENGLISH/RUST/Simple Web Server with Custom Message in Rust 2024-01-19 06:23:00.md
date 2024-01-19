```rust
// This Rust code creates a simple web server that responds to HTTP requests with a custom message.

use std::{net::TcpListener, io::{Read, Write}};

fn main() {
    // Create a new TCP listener on port 8080.
    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

    // Accept incoming connections and handle them in a loop.
    for stream in listener.incoming() {
        // Get the stream from the connection.
        let mut stream = stream.unwrap();

        // Create a buffer to store the HTTP request.
        let mut request = [0; 1024];

        // Read the HTTP request from the stream.
        stream.read(&mut request).unwrap();

        // Create a response message.
        let response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello, world!";

        // Write the response message to the stream.
        stream.write(response.as_bytes()).unwrap();

        // Flush the stream to send the response.
        stream.flush().unwrap();
    }
}
```

This Rust code creates a simple web server that listens for incoming HTTP requests on port 8080. When it receives a request, it responds with a custom message.

Here's a detailed explanation of the code:

1. `use std::{net::TcpListener, io::{Read, Write}};`: This line imports the necessary standard library modules for working with TCP connections and input/output.

2. `fn main() { ... }`: This is the main function of the program, where the execution starts.

3. `let listener = TcpListener::bind("127.0.0.1:8080").unwrap();`: This line creates a new TCP listener on port 8080 using the `bind` method on the `TcpListener` type. The `unwrap()` method is used to handle any errors that may occur during the binding process.

4. `for stream in listener.incoming() { ... }`: This line creates a loop that listens for incoming connections on the TCP listener. The `incoming` method returns an iterator that yields incoming streams when a new connection is established.

5. `let mut stream = stream.unwrap();`: This line gets the stream object from the connection and makes it mutable.

6. `let mut request = [0; 1024];`: This line creates a buffer of 1024 bytes to store the HTTP request. The `[0; 1024]` syntax creates an array of 1024 zero bytes.

7. `stream.read(&mut request).unwrap();`: This line reads the HTTP request from the stream and stores it in the `request` buffer. The `unwrap()` method is used to handle any errors that may occur during the reading process.

8. `let response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello, world!";`: This line creates the HTTP response message. The response includes the status code (200 OK), the Content-Type header (text/plain), and the custom message "Hello, world!".

9. `stream.write(response.as_bytes()).unwrap();`: This line writes the HTTP response message to the stream. The `as_bytes()` method converts the `response` string into a byte array, which is suitable for writing to the stream. The `unwrap()` method is used to handle any errors that may occur during the writing process.

10. `stream.flush().unwrap();`: This line flushes the stream to send the HTTP response. The `flush()` method ensures that all data in the stream is sent to the client. The `unwrap()` method is used to handle any errors that may occur during the flushing process.

This code provides a basic example of how to create a simple web server in Rust using the standard library. It can be extended and customized to handle more complex HTTP requests and responses.