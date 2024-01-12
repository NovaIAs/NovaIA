```rust
// This is a Rust program that implements a simple HTTP server.

// First, we need to import the necessary libraries.
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::thread;

// Next, we define a function to handle each HTTP request.
fn handle_request(mut stream: TcpStream) {
    // First, we read the request from the stream.
    let mut buffer = [0; 1024];
    stream.read(&mut buffer).unwrap();

    // Next, we parse the request into a string.
    let request = String::from_utf8_lossy(&buffer);

    // Then, we check if the request is valid.
    if request.starts_with("GET /") {
        // If the request is valid, we send a response back to the client.
        let response = "HTTP/1.1 200 OK\n\
            Content-Type: text/html\n\
            Content-Length: 11\n\
            \n\
            Hello world!";

        stream.write_all(response.as_bytes()).unwrap();
    } else {
        // If the request is not valid, we send a 404 error back to the client.
        let response = "HTTP/1.1 404 Not Found\n\
            Content-Type: text/html\n\
            Content-Length: 13\n\
            \n\
            404 Not Found";

        stream.write_all(response.as_bytes()).unwrap();
    }
}

// Finally, we start the server.
fn main() {
    // First, we create a TCP listener on port 8080.
    let listener = TcpListener::bind("0.0.0.0:8080").unwrap();

    // Then, we loop forever, accepting new connections.
    loop {
        // We accept the next connection.
        let (stream, _) = listener.accept().unwrap();

        // We spawn a new thread to handle the request.
        thread::spawn(move || {
            handle_request(stream);
        });
    }
}
```

This code implements a simple HTTP server in Rust. The server listens on port 8080 and responds to GET requests with a "Hello world!" message.

The code is divided into three parts:

1. The first part imports the necessary libraries.
2. The second part defines a function to handle each HTTP request.
3. The third part starts the server.

The `handle_request()` function is the heart of the server. It reads the request from the stream, parses it into a string, checks if it is valid, and sends a response back to the client.

The `main()` function starts the server. It creates a TCP listener on port 8080 and then loops forever, accepting new connections. For each new connection, it spawns a new thread to handle the request.

This code is a good example of how to use Rust to write a simple network server.