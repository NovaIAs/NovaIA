```tcl
# This Tcl script implements a simple chat server.
# It listens on a port for incoming connections and forwards messages between clients.

# Create a new TCP socket.
set sock [socket -server 12345]

# Create a list to store the connected clients.
set clients {}

# Create a loop to listen for incoming connections.
while {1} {
    # Accept a new connection.
    set new_client [socket -accept $sock]

    # Add the new client to the list of clients.
    lappend clients $new_client

    # Create a loop to listen for messages from the new client.
    while {1} {
        # Receive a message from the new client.
        set msg [socket -recv $new_client]

        # If the message is empty, the client has disconnected.
        if {$msg == ""} {
            # Remove the client from the list of clients.
            lremove clients $new_client

            # Close the client's socket.
            socket -close $new_client

            # Break out of the loop.
            break
        }

        # Send the message to all other clients.
        foreach client $clients {
            if {$client != $new_client} {
                socket -send $client $msg
            }
        }
    }
}

# Close the server socket.
socket -close $sock
```

Explanation:

* The script creates a new TCP socket on port 12345 using the `socket -server` command.
* It creates a list called `clients` to store the connected clients.
* It enters a loop to listen for incoming connections using the `while {1}` construct.
* Inside the loop, it accepts a new connection using the `socket -accept` command and adds it to the `clients` list.
* It then enters another loop to listen for messages from the new client.
* Inside the loop, it receives a message from the new client using the `socket -recv` command.
* If the message is empty, it means the client has disconnected, so it removes the client from the `clients` list, closes the client's socket, and breaks out of the loop.
* If the message is not empty, it sends the message to all other clients using the `foreach` command.
* Once all clients have received the message, the loop continues to listen for more messages from the new client.
* When the server is no longer needed, the `socket -close` command is used to close the server socket.