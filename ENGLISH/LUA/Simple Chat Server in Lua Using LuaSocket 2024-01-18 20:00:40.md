```
-- This Lua code implements a simple chat server using the LuaSocket library.

-- Load the LuaSocket library.
package.path = package.path .. ";./?.lua"
require "socket"

-- Create a new TCP server socket.
local server_socket = socket.tcp()

-- Bind the server socket to a specific IP address and port.
server_socket:bind("127.0.0.1", 8080)

-- Start listening for incoming connections.
server_socket:listen()

-- Create a table to store the connected clients.
local clients = {}

-- Main loop: accept incoming connections and handle client messages.
while true do
    -- Accept an incoming connection.
    local client_socket, client_address = server_socket:accept()

    -- Add the new client to the table of connected clients.
    clients[client_address] = client_socket

    -- Create a new thread to handle messages from the client.
    local client_thread = coroutine.create(function()
        -- Loop until the client closes the connection.
        while true do
            -- Receive a message from the client.
            local message = client_socket:receive()

            -- If the message is nil, the client has closed the connection.
            if message == nil then
                break
            end

            -- Broadcast the message to all other connected clients.
            for other_client_address, other_client_socket in pairs(clients) do
                if other_client_address ~= client_address then
                    other_client_socket:send(message)
                end
            end
        end

        -- Remove the client from the table of connected clients.
        clients[client_address] = nil
    end)

    -- Resume the client thread, starting the message handling loop.
    coroutine.resume(client_thread)
end
```

Explanation of the code:

1. The first line of the code imports the LuaSocket library, which is required for creating and managing network sockets.

2. The second line creates a new TCP server socket using the `socket.tcp()` function. This socket will be used to listen for incoming client connections.

3. The third line binds the server socket to a specific IP address and port using the `:bind()` method. In this case, the server will listen on the loopback address (127.0.0.1) and port 8080.

4. The fourth line starts listening for incoming connections using the `:listen()` method. The server will now wait for clients to connect.

5. The fifth line creates a table called `clients` to store the connected clients. The table will be indexed by the client's IP address.

6. The main loop starts on the sixth line. This loop will run forever, accepting incoming connections and handling client messages.

7. Inside the main loop, the server accepts an incoming connection using the `:accept()` method. This method returns a new socket object representing the client connection, as well as the client's IP address.

8. The new client is added to the `clients` table, using the client's IP address as the key.

9. A new thread is created to handle messages from the client. This is done using the `coroutine.create()` function. The thread function is defined as an anonymous function (lambda).

10. Inside the client thread, the message handling loop starts. This loop will run until the client closes the connection.

11. In the message handling loop, a message is received from the client using the `:receive()` method. If the message is `nil`, it means that the client has closed the connection.

12. If the message is not `nil`, it is broadcasted to all other connected clients using a for loop. The message is sent to each client's socket using the `:send()` method.

13. When the client closes the connection, the client thread exits and the client is removed from the `clients` table.

14. The main loop then continues, accepting new incoming connections and handling client messages.