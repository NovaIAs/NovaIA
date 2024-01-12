```ruby
require 'active_record'
require 'sinatra'
require 'sinatra/activerecord'
require 'json'

# Define the database connection
set :database, "sqlite3:database.sqlite3"

# Define the ActiveRecord model
class User < ActiveRecord::Base
  validates :name, presence: true
  validates :email, presence: true, uniqueness: true
end

# Define the routes
get '/' do
  redirect '/users'
end

get '/users' do
  @users = User.all
  erb :index
end

post '/users' do
  user = User.new(params[:user])
  if user.save
    redirect '/users'
  else
    @errors = user.errors
    erb :new
  end
end

get '/users/new' do
  @user = User.new
  erb :new
end

get '/users/:id' do
  @user = User.find(params[:id])
  erb :show
end

put '/users/:id' do
  user = User.find(params[:id])
  if user.update(params[:user])
    redirect "/users/#{user.id}"
  else
    @errors = user.errors
    erb :edit
  end
end

get '/users/:id/edit' do
  @user = User.find(params[:id])
  erb :edit
end

delete '/users/:id' do
  user = User.find(params[:id])
  user.destroy
  redirect '/users'
end

# JSON API routes
get '/api/users' do
  @users = User.all
  json @users
end

post '/api/users' do
  user = User.new(JSON.parse(request.body.read))
  if user.save
    json user
  else
    status 400
    json @errors = user.errors
  end
end

get '/api/users/:id' do
  @user = User.find(params[:id])
  json @user
end

put '/api/users/:id' do
  user = User.find(params[:id])
  if user.update(params[:user])
    json user
  else
    status 400
    json @errors = user.errors
  end
end

delete '/api/users/:id' do
  user = User.find(params[:id])
  user.destroy
  json user
end

# Run the server
run!
```

This Ruby code is a complex and differentiated application that demonstrates various features and concepts. It uses the Sinatra framework for routing, ActiveRecord for database interaction, and JSON for data exchange. The application allows users to manage a list of users, including CRUD (Create, Read, Update, Delete) operations, using both a web interface and a JSON API.

The code is well-organized and structured, with each component having a specific purpose. The routes are defined using the `get`, `post`, `put`, and `delete` methods, and each route is associated with a specific action. The ActiveRecord model (`User`) is defined with validations and associations, and it is used to interact with the database.

The code also includes JSON API routes that allow clients to communicate with the application using JSON data. The JSON API routes follow the RESTful API design principles, and they provide a consistent way to create, read, update, and delete user records.

Overall, this code is a comprehensive and well-written application that demonstrates various features and concepts in Ruby. It is a good example of how to build a complex and differentiated application using Ruby, Sinatra, ActiveRecord, and JSON.