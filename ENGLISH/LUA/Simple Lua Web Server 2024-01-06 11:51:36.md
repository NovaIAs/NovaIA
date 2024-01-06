```lua
-- This Lua script implements a simple web server that responds to HTTP requests.

-- Load the required Lua modules.
local socket = require("socket")
local http = require("http")

-- Create a new TCP socket.
local sock = socket.tcp()

-- Bind the socket to the specified port.
sock:bind("*", 80)

-- Listen for incoming connections.
sock:listen()

-- Loop forever, accepting new connections and handling requests.
while true do
  -- Accept the next incoming connection.
  local conn, addr = sock:accept()

  -- Read the HTTP request from the connection.
  local request = conn:receive()

  -- Parse the HTTP request.
  local method, path, version = http.parseRequest(request)

  -- Handle the request.
  if method == "GET" and path == "/" then
    -- Send a simple HTML response.
    conn:send("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<h1>Hello, world!</h1>")
  else
    -- Send a 404 Not Found response.
    conn:send("HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n404 Not Found")
  end

  -- Close the connection.
  conn:close()
end
```

This code creates a simple web server that responds to HTTP requests. The server listens on port 80 and accepts incoming connections. For each connection, the server receives the HTTP request and parses it. The server then handles the request, either by sending a simple HTML response or a 404 Not Found response. Finally, the server closes the connection.

Here is a more detailed explanation of the code:

* The first few lines of the code load the required Lua modules. The `socket` module provides functions for working with sockets, and the `http` module provides functions for parsing and handling HTTP requests.
* The next line creates a new TCP socket. A socket is a communication endpoint that allows two programs to communicate with each other.
* The next line binds the socket to the specified port. This tells the operating system that the server is listening for incoming connections on port 80.
* The next line listens for incoming connections. This puts the server in a state where it is ready to accept new connections.
* The next line is a loop that runs forever. This loop is the main event loop of the server. It accepts new connections and handles requests.
* The next line accepts the next incoming connection. This returns a new socket object that represents the connection, as well as the address of the client that made the connection.
* The next line reads the HTTP request from the connection. This returns a string containing the HTTP request.
* The next line parses the HTTP request. This splits the request into its components, such as the method, path, and version.
* The next line handles the request. This is where the server decides what to do with the request. In this case, the server either sends a simple HTML response or a 404 Not Found response.
* The next line closes the connection. This tells the operating system that the server is finished with the connection.
* The loop then repeats, waiting for the next incoming connection.