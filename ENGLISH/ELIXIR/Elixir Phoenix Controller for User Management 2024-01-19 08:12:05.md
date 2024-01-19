```elixir
defmodule MyApp.Controller do
  use Plug.Router

  plug :match

  get "/" do
    text "Root!"
  end

  get "/users" do
    users = MyApp.User.all
    json users
  end

  get "/users/:id" do
    user = MyApp.User.find(params["id"])
    json user
  end

  post "/users" do
    user = MyApp.User.create(params["user"])
    json user
  end

  put "/users/:id" do
    user = MyApp.User.update(params["id"], params["user"])
    json user
  end

  delete "/users/:id" do
    MyApp.User.delete(params["id"])
    json %{message: "User deleted!"}
  end
end
```

This code defines an Elixir Phoenix controller, which handles HTTP requests and responses.

The `defmodule MyApp.Controller do` line defines the module name and its dependencies. In this case, the controller depends on the `Plug.Router` module, which provides routing functionality.

The `use Plug.Router` line tells Elixir to use the `Plug.Router` module in this controller. This allows the controller to define routes and handle HTTP requests.

The `plug :match` line tells Elixir to use the `Plug.Match` plug in this controller. This plug will match the incoming HTTP request with the defined routes and call the appropriate action function.

The `get "/", do: text "Root!"` line defines a route for the root URL "/". When a GET request is made to the root URL, the `text "Root!"` function will be called. This function simply sends a text response with the message "Root!".

The `get "/users", do: json MyApp.User.all` line defines a route for the "/users" URL. When a GET request is made to the "/users" URL, the `json MyApp.User.all` function will be called. This function calls the `MyApp.User.all()` function to get all the users from the database and then sends a JSON response with the users data.

The `get "/users/:id", do: json MyApp.User.find(params["id"])` line defines a route for the "/users/:id" URL, where `:id` is a placeholder for the user ID. When a GET request is made to the "/users/:id" URL, the `json MyApp.User.find(params["id"])` function will be called. This function calls the `MyApp.User.find(params["id"])` function to find the user with the specified ID from the database and then sends a JSON response with the user data.

The `post "/users", do: json MyApp.User.create(params["user"])` line defines a route for the "/users" URL. When a POST request is made to the "/users" URL, the `json MyApp.User.create(params["user"])` function will be called. This function calls the `MyApp.User.create(params["user"])` function to create a new user in the database with the data provided in the request body and then sends a JSON response with the created user data.

The `put "/users/:id", do: json MyApp.User.update(params["id"], params["user"])` line defines a route for the "/users/:id" URL, where `:id` is a placeholder for the user ID. When a PUT request is made to the "/users/:id" URL, the `json MyApp.User.update(params["id"], params["user"])` function will be called. This function calls the `MyApp.User.update(params["id"], params["user"])` function to update the user with the specified ID in the database with the data provided in the request body and then sends a JSON response with the updated user data.

The `delete "/users/:id", do: json %{message: "User deleted!"}` line defines a route for the "/users/:id" URL, where `:id` is a placeholder for the user ID. When a DELETE request is made to the "/users/:id" URL, the `json %{message: "User deleted!"}` function will be called. This function calls the `MyApp.User.delete(params["id"])` function to delete the user with the specified ID from the database and then sends a JSON response with a message indicating that the user was deleted.