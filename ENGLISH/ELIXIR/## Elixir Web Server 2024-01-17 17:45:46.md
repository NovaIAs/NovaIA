```elixir
# Create a simple web server

defmodule MyApp.WebServer do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello, world!")
  end

  get "/hello/:name" do
    name = Plug.Conn.fetch_param(conn, "name")

    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello, #{name}!")
  end
end

# Initialize the web server

defmodule MyApp do
  def main(_) do
    {:ok, _} = MyApp.WebServer.start()

    IO.puts "Server is running at http://localhost:8080"
  end
end

# Start the application

Application.start(:myapp, MyApp, [])

```

## Explanation:

1. **Modules:**
   - `MyApp.WebServer`: This module defines the web server.
   - `MyApp`: This module defines the application's main function.

2. **Plug.Router:**
   - The `use Plug.Router` line imports the `Plug.Router` behavior, which allows us to define routes for the web server.

3. **Plugs:**
   - `plug :match`: This plug matches the incoming request against the defined routes.
   - `plug :dispatch`: This plug dispatches the request to the appropriate route handler.

4. **Routes:**
   - `get "/"`: This route matches the root URL (`/`) and handles GET requests.
   - `get "/hello/:name"`: This route matches the URL `/hello/:name` and handles GET requests. The `:name` part is a parameter that will be passed to the route handler.

5. **Route Handlers:**
   - The `conn` variable represents the Plug connection, which contains information about the request and response.
   - `put_resp_content_type/2`: This function sets the content type of the response.
   - `send_resp/3`: This function sends the response back to the client.

6. **Application:**
   - The `Application.start(:myapp, MyApp, [])` line starts the application with the `MyApp` module as the main module and an empty list of arguments.

7. **Main Function:**
   - The `def main(_)` function is the main function of the application. It starts the web server and prints a message to the console.