```ruby
# This code implements a simple web server in Ruby. It listens on port 8080 and
# responds to requests with a simple text message.

require 'socket'

# Create a TCP server socket
server = TCPServer.new('0.0.0.0', 8080)

# Loop forever, accepting new connections
loop do
  # Wait for a client to connect
  client = server.accept

  # Read the request from the client
  request = client.gets

  # Parse the request to get the requested path
  path = request.split(' ')[1]

  # Construct a response based on the requested path
  response = case path
            when '/'
              "Hello, world!"
            when '/about'
              "This is a simple web server written in Ruby."
            else
              "404 Not Found"
            end

  # Send the response to the client
  client.puts response

  # Close the connection with the client
  client.close
end

# This code explains the code above:

# 1. We start by requiring the 'socket' library, which provides the functionality
#    for creating and managing sockets in Ruby.

# 2. Next, we create a TCP server socket using the TCPServer.new method. The
#    TCPServer.new method takes two arguments: the IP address to listen on (in this
#    case, '0.0.0.0', which means all IP addresses) and the port to listen on
#    (in this case, 8080).

# 3. The server.accept method is used to wait for a client to connect to the server.
#    When a client connects, the accept method returns a client socket object, which
#    represents the connection between the server and the client.

# 4. We use the client.gets method to read the request from the client. The gets
#    method reads a single line of text from the client socket.

# 5. We parse the request to get the requested path. The request is a string that
#    contains the HTTP method, the requested path, and the HTTP version. We use
#    the split method to split the request into an array of strings, and then we
#    select the second element of the array, which is the requested path.

# 6. We construct a response based on the requested path. In this case, we have
#    three possible responses:

#     * If the requested path is '/', we respond with "Hello, world!".
#     * If the requested path is '/about', we respond with "This is a simple web
#       server written in Ruby.".
#     * If the requested path is anything else, we respond with "404 Not Found".

# 7. We use the client.puts method to send the response to the client. The puts
#    method writes a string to the client socket.

# 8. Finally, we use the client.close method to close the connection with the client.
#    This is important to do, otherwise the server will keep the connection open
#    even if the client has disconnected.
```