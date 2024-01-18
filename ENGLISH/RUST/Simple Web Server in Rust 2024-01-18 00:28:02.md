```rust
// This is a Rust program that simulates a simple web server.
// It listens on port 8080 and responds to GET requests with a simple HTML page.

// Import the necessary libraries.
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};

// Define the main function.
fn main() {
    // Create a new TCP listener on port 8080.
    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

    // Loop forever, listening for incoming connections.
    loop {
        // Accept the next incoming connection.
        let (mut stream, _) = listener.accept().unwrap();

        // Read the request from the client.
        let mut request = [0; 1024];
        stream.read(&mut request).unwrap();

        // Parse the request.
        let request_line = std::str::from_utf8(&request).unwrap();
        let parts: Vec<&str> = request_line.split(' ').collect();

        // Check if the request is a GET request.
        if parts[0] == "GET" {
            // Send a simple HTML page back to the client.
            let response = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<h1>Hello, world!</h1>";
            stream.write(response.as_bytes()).unwrap();
        } else {
            // Send a 404 error page back to the client.
            let response = "HTTP/1.1 404 Not Found\r\nContent-Type: text/html\r\n\r\n<h1>404 Not Found</h1>";
            stream.write(response.as_bytes()).unwrap();
        }

        // Close the connection.
        stream.shutdown(std::net::Shutdown::Both).unwrap();
    }
}
```

This code creates a simple web server that listens on port 8080. When a client connects to the server, the server reads the request from the client and parses it. If the request is a GET request, the server sends a simple HTML page back to the client. If the request is not a GET request, the server sends a 404 error page back to the client. The server then closes the connection.

Here is a breakdown of the code:

* The `use` statements import the necessary libraries.
* The `main` function is the entry point for the program.
* The `TcpListener::bind` function creates a new TCP listener on port 8080.
* The `loop` statement loops forever, listening for incoming connections.
* The `accept` function accepts the next incoming connection.
* The `read` function reads the request from the client.
* The `std::str::from_utf8` function converts the request from a byte array to a string.
* The `split` function splits the request string into parts.
* The `if` statement checks if the request is a GET request.
* The `write` function sends the response back to the client.
* The `shutdown` function closes the connection.

This code is a good example of how to write a simple web server in Rust. It is also a good example of how to use the `std` library to work with TCP sockets.