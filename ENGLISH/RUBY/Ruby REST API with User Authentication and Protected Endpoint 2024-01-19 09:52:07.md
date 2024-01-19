```ruby
# Import the necessary gems
require 'sinatra'
require 'json'
require 'mongoid'
require 'bcrypt'

# Connect to the MongoDB database
Mongoid.load!("mongoid.yml")

# Define the User model
class User
  include Mongoid::Document
  field :username, type: String
  field :password, type: String
  field :email, type: String

  # Use bcrypt to hash the password before saving the user
  before_save do |user|
    user.password = BCrypt::Password.create(user.password)
  end
end

# Define the API endpoints

# The root endpoint
get '/' do
  "Hello, world!"
end

# The users endpoint
get '/users' do
  users = User.all
  users.to_json
end

# The create user endpoint
post '/users' do
  user = User.new(JSON.parse(request.body.read))
  if user.save
    status 201
    user.to_json
  else
    status 400
    { errors: user.errors }.to_json
  end
end

# The login endpoint
post '/login' do
  user = User.find_by(username: params[:username])
  if user && BCrypt::Password.new(user.password) == params[:password]
    status 200
    { token: JWT.encode({ id: user.id }, 'secret') }.to_json
  else
    status 401
    { errors: ['Invalid credentials'] }.to_json
  end
end

# The protected endpoint
get '/protected' do
  authenticate!
  "You are authenticated!"
end

# Helper method to authenticate the user
def authenticate!
  unless current_user
    status 401
    { errors: ['Unauthenticated'] }.to_json
  end
end

# Helper method to get the current user
def current_user
  @_current_user ||= User.find_by(id: JWT.decode(request.headers['Authorization'], 'secret')[0]['id'])
end

# Start the Sinatra application
run!
```

This Ruby code creates a simple REST API using the Sinatra framework. It allows users to create accounts, log in, and access a protected endpoint. The code is explained below:

1. **Import the necessary gems**: This line imports the necessary Ruby gems for the application to work, including Sinatra, JSON, Mongoid, and BCrypt.

2. **Connect to the MongoDB database**: This line connects the application to the MongoDB database using the Mongoid library.

3. **Define the User model**: This defines the User model using the Mongoid library. The User model has three fields: username, password, and email.

4. **Use bcrypt to hash the password before saving the user**: This ensures that passwords are stored securely in the database.

5. **Define the API endpoints**: This section defines the API endpoints for the application. There are four endpoints:
    * The root endpoint ("/") returns a simple "Hello, world!" message.
    * The users endpoint ("/users") returns a list of all users in the database.
    * The create user endpoint ("/users") creates a new user and returns the newly created user.
    * The login endpoint ("/login") allows users to log in and returns a JSON Web Token (JWT) that can be used to access protected endpoints.
    * The protected endpoint ("/protected") is a protected endpoint that can only be accessed by authenticated users.

6. **Helper methods to authenticate the user and get the current user**: These helper methods are used to authenticate the user and get the current user's information.

7. **Start the Sinatra application**: This line starts the Sinatra application.

This code creates a simple REST API that allows users to create accounts, log in, and access a protected endpoint. It also uses bcrypt to securely store passwords in the database. This code can be used as a starting point for building more complex REST APIs in Ruby.