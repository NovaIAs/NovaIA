```ruby
# Require the necessary libraries
require 'sinatra'
require 'sinatra/reloader' if development?
require 'json'
require 'sinatra/activerecord'
require 'active_record'

# Establish a connection to the database
set :database, "sqlite3:database.sqlite3"

# Define the Game model
class Game < ActiveRecord::Base
  # Define the attributes of the Game model
  attr_accessor :name, :description, :price
  # Define the validations for the Game model
  validates :name, presence: true
  validates :description, presence: true
  validates :price, presence: true
end

# Define the routes for the application
get '/' do
  # Render the index view
  erb :index
end

get '/games' do
  # Find all of the games in the database
  @games = Game.all
  # Render the games view
  erb :games
end

get '/games/:id' do
  # Find the game with the specified ID
  @game = Game.find(params[:id])
  # Render the game view
  erb :game
end

post '/games' do
  # Create a new game with the specified attributes
  @game = Game.create(params[:game])
  # Redirect to the game's page
  redirect to "/games/#{@game.id}"
end

put '/games/:id' do
  # Find the game with the specified ID
  @game = Game.find(params[:id])
  # Update the game's attributes
  @game.update(params[:game])
  # Redirect to the game's page
  redirect to "/games/#{@game.id}"
end

delete '/games/:id' do
  # Find the game with the specified ID
  @game = Game.find(params[:id])
  # Delete the game
  @game.destroy
  # Redirect to the games page
  redirect to '/games'
end

# Start the server
run!
```

This code creates a simple Ruby web application that allows users to manage a collection of games. The application uses the Sinatra framework and the ActiveRecord ORM to interact with a SQLite database.

The code starts by requiring the necessary libraries. The `sinatra` library is used to create the web application, the `sinatra/reloader` library is used to automatically reload the application when changes are made to the code, the `json` library is used to parse JSON data, the `sinatra/activerecord` library is used to integrate ActiveRecord with Sinatra, and the `active_record` library is used to interact with the database.

The `set :database` line establishes a connection to the SQLite database. The `sqlite3:database.sqlite3` string specifies the type of database (SQLite) and the name of the database file (`database.sqlite3`).

The `Game` class is defined next. This class represents a game in the database. The `attr_accessor` lines specify the attributes of the `Game` class. The `validates` lines specify the validations that must be performed on the attributes before a game can be saved to the database.

The `get '/'` route renders the index view. This view is the main page of the application.

The `get '/games'` route finds all of the games in the database and renders the `games` view. This view displays a list of all of the games in the database.

The `get '/games/:id'` route finds the game with the specified ID and renders the `game` view. This view displays the details of a single game.

The `post '/games'` route creates a new game with the specified attributes and redirects to the game's page.

The `put '/games/:id'` route updates the game with the specified ID with the specified attributes and redirects to the game's page.

The `delete '/games/:id'` route deletes the game with the specified ID and redirects to the games page.

The `run!` line starts the server.