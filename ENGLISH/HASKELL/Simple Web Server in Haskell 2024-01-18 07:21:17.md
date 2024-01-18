```haskell
-- This is a Haskell program that implements a simple web server.

-- Import the necessary modules.
import Network.Socket
import Network.Socket.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import System.IO

-- Define the port on which the server will listen for connections.
port :: Int
port = 8080

-- Define the function that will handle each client connection.
handleConnection :: Socket -> IO ()
handleConnection sock = do
  -- Receive the request from the client.
  request <- recv sock 1024

  -- Convert the request to a string.
  requestStr <- ByteString.unpack request

  -- Parse the request.
  (method, path, _) <- parseRequest requestStr

  -- Get the file specified by the path.
  file <- readFile path

  -- Convert the file to a byte string.
  fileBytes <- ByteString.pack file

  -- Send the response to the client.
  send sock $ getResponse method fileBytes

-- Define the function that will parse a request.
parseRequest :: String -> (String, String, String)
parseRequest request =
  let (method, rest) = break (' ' :) request
      (path, version) = break (' ' :) rest
   in (method, path, version)

-- Define the function that will generate a response.
getResponse :: String -> ByteString -> String
getResponse method fileBytes =
  let statusLine = "HTTP/1.1 200 OK"
      headers = "Content-Type: text/html\r\nContent-Length: " ++ show (ByteString.length fileBytes) ++ "\r\n\r\n"
   in statusLine ++ headers ++ fileBytes

-- Define the main function.
main :: IO ()
main = do
  -- Create a socket.
  sock <- socket AF_INET Stream defaultProtocol

  -- Bind the socket to the port.
  bind sock (SockAddrInet port INADDR_ANY)

  -- Listen for connections on the socket.
  listen sock 5

  -- Accept connections from clients.
  forever $ do
    (clientSock, _) <- accept sock

    -- Handle each connection in a separate thread.
    forkIO $ handleConnection clientSock
```

This code is a simple web server written in Haskell. The server listens for connections on port 8080 and responds to requests with the contents of the file specified in the request path.

The server is implemented using the `Network.Socket` and `Network.Socket.ByteString` modules. The `handleConnection` function is responsible for handling each client connection. It receives the request from the client, parses the request, gets the file specified by the path, and sends the response to the client.

The `parseRequest` function parses the request into a tuple containing the method, path, and version. The `getResponse` function generates a response containing the status line, headers, and file contents.

The main function creates a socket, binds the socket to the port, listens for connections on the socket, and accepts connections from clients. Each connection is handled in a separate thread.