```ruby
# This code creates a simple web server using Sinatra and provides an interactive command-line interface for user input
require 'sinatra'

# Define the root route
get '/' do
  # Display a welcome message and instructions to the user
  "Welcome to the Interactive Command-Line Server!\nType 'help' for a list of available commands."
end

# Define the help route
get '/help' do
  # Display a list of available commands and their descriptions
  "Available commands:\n\nhelp: Displays this help message.\nquit: Exits the server.\ntime: Displays the current time.\nname: Prompts the user for their name and greets them.\nsum: Prompts the user for two numbers and displays their sum."
end

# Define the quit route
get '/quit' do
  # Stop the server
  halt
end

# Define the time route
get '/time' do
  # Get the current time and display it
  "The current time is #{Time.now}."
end

# Define the name route
get '/name' do
  # Prompt the user for their name
  name = ask("What is your name?")

  # Greet the user by name
  "Hello, #{name}! Welcome to the server."
end

# Define the sum route
get '/sum' do
  # Prompt the user for two numbers
  num1 = ask("Enter the first number:")
  num2 = ask("Enter the second number:")

  # Calculate and display the sum of the two numbers
  "The sum of #{num1} and #{num2} is #{num1.to_i + num2.to_i}."
end

# Start the server
run!
```

**Explanation:**

1. We start by requiring the Sinatra library, which is a simple and easy-to-use web framework for Ruby.

2. We define the root route ("/") with a GET request handler. This route displays a welcome message and instructions to the user.

3. We define a help route ("/help") with a GET request handler. This route displays a list of available commands and their descriptions.

4. We define a quit route ("/quit") with a GET request handler. This route stops the server.

5. We define a time route ("/time") with a GET request handler. This route gets the current time and displays it.

6. We define a name route ("/name") with a GET request handler. This route prompts the user for their name and greets them.

7. We define a sum route ("/sum") with a GET request handler. This route prompts the user for two numbers, calculates their sum, and displays it.

8. Finally, we start the server using the `run!` method.

This code creates a simple web server with an interactive command-line interface. The user can access various commands by visiting different routes on the server. For example, visiting "/help" will display a list of available commands, "/time" will display the current time, "/name" will greet the user by name, and "/sum" will calculate and display the sum of two numbers.