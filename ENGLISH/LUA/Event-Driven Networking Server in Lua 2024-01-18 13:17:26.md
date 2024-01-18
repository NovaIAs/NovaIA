```lua
-- This Lua script implements a basic event-driven networking server.

-- Create a new socket object.
local socket = require("socket")

-- Create a new TCP server socket.
local server = socket.tcp()

-- Bind the server socket to a specific IP address and port.
server:bind("0.0.0.0", 8080)

-- Listen for incoming connections.
server:listen()

-- Create a table to store the connected clients.
local clients = {}

-- Define a function to handle incoming connections.
function handle_connection(client)
  -- Add the client to the table of connected clients.
  clients[#clients + 1] = client

  -- Create a new thread to handle this client.
  coroutine.create(function()
    -- Loop until the client disconnects.
    while true do
      -- Receive data from the client.
      local data = client:receive()

      -- If the client disconnected, remove it from the table of connected clients.
      if data == nil then
        for i, c in pairs(clients) do
          if c == client then
            table.remove(clients, i)
          end
        end
        return
      end

      -- Process the data from the client.

      -- Send a response to the client.
      client:send("Hello, world!")
    end
  end)
end

-- Loop forever, accepting new connections and handling them.
while true do
  -- Accept a new connection.
  local client, addr = server:accept()

  -- Handle the connection.
  handle_connection(client)
end
```

This script creates a simple TCP server that listens for incoming connections on port 8080. When a client connects to the server, a new thread is created to handle the client. The thread receives data from the client, processes it, and sends a response back to the client. The server continues to accept new connections and handle them in this way indefinitely.

Here is a breakdown of the code:

* The `require("socket")` line imports the Lua socket library, which provides functions for working with network sockets.
* The `local server = socket.tcp()` line creates a new TCP server socket.
* The `server:bind("0.0.0.0", 8080)` line binds the server socket to the IP address "0.0.0.0" (which means all IP addresses) and port 8080.
* The `server:listen()` line starts the server listening for incoming connections.
* The `local clients = {}` line creates a table to store the connected clients.
* The `handle_connection(client)` function is defined to handle incoming connections.
* The `clients[#clients + 1] = client` line adds the client to the table of connected clients.
* The `coroutine.create(function()` line creates a new thread to handle the client.
* The `while true do` loop in the thread receives data from the client, processes it, and sends a response back to the client.
* The `if data == nil then` line checks if the client disconnected.
* The `for i, c in pairs(clients) do` loop removes the client from the table of connected clients if it disconnected.
* The `return` line exits the thread.
* The `while true do` loop in the main script accepts new connections and handles them by calling the `handle_connection()` function.