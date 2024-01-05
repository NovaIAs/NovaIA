```lua
-- This code implements a simple server that listens on port 8080 and responds to HTTP requests.

-- Require the 'socket' library, which provides functions for working with sockets.
local socket = require("socket")

-- Create a TCP socket.
local server = socket.tcp()

-- Bind the socket to port 8080.
server:bind("*", 8080)

-- Listen for incoming connections.
server:listen()

-- Create a loop that will handle incoming connections.
while true do
  -- Accept the next incoming connection.
  local client, _ = server:accept()

  -- Create a new thread to handle the client connection.
  local thread = coroutine.create(function()
    -- Read the client's request.
    local request = client:receive()

    -- Parse the request.
    local method, path, version = request:match("^([A-Z]+) ([^ ]+) HTTP/([0-9.]+)$")

    -- Send a response to the client.
    if method == "GET" and path == "/" then
      client:send("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello, world!")
    else
      client:send("HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n404 Not Found")
    end

    -- Close the client connection.
    client:close()
  end)

  -- Resume the thread to start handling the client connection.
  coroutine.resume(thread)
end
```

This code creates a simple HTTP server that listens on port 8080. When a client connects to the server, the server accepts the connection and creates a new thread to handle the client request. The thread reads the client's request, parses it, and sends a response back to the client. The server then closes the client connection.

Here is a breakdown of the code:

* The `socket` library is required to work with sockets.
* A TCP socket is created using the `socket.tcp()` function.
* The socket is bound to port 8080 using the `:bind()` method.
* The socket is set to listen for incoming connections using the `:listen()` method.
* A loop is created to handle incoming connections.
* The `:accept()` method is used to accept the next incoming connection.
* A new thread is created using the `coroutine.create()` function to handle the client connection.
* The thread reads the client's request using the `:receive()` method.
* The request is parsed using the `:match()` method to extract the method, path, and version.
* A response is sent back to the client using the `:send()` method.
* The client connection is closed using the `:close()` method.
* The thread is resumed to start handling the client connection.

This code is a simple example of how to create a server in Lua. It can be used as a starting point for creating more complex servers.