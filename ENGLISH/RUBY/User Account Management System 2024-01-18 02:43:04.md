```ruby
# Define a module for managing user accounts
module UserAccounts
  # Class representing a user account
  class Account
    # Initialize the account with a username and password
    def initialize(username, password)
      @username = username
      @password = password
    end

    # Get the username
    def username
      @username
    end

    # Get the password
    def password
      @password
    end

    # Validate the password against the given input
    def validate_password(input)
      @password == input
    end
  end

  # Class representing a user account repository
  class Repository
    # Initialize the repository with an array of accounts
    def initialize(accounts)
      @accounts = accounts
    end

    # Find an account by its username
    def find_by_username(username)
      @accounts.find { |account| account.username == username }
    end

    # Add a new account to the repository
    def add(account)
      @accounts << account
    end

    # Remove an account from the repository
    def remove(account)
      @accounts.delete(account)
    end
  end

  # Class representing a user account service
  class Service
    # Initialize the service with a repository
    def initialize(repository)
      @repository = repository
    end

    # Authenticate a user by username and password
    def authenticate(username, password)
      account = @repository.find_by_username(username)
      account && account.validate_password(password)
    end

    # Create a new user account
    def create(username, password)
      account = Account.new(username, password)
      @repository.add(account)
    end

    # Delete a user account
    def delete(username)
      account = @repository.find_by_username(username)
      @repository.remove(account)
    end
  end
end

# Usage example
repository = UserAccounts::Repository.new([
  UserAccounts::Account.new("user1", "password1"),
  UserAccounts::Account.new("user2", "password2")
])
service = UserAccounts::Service.new(repository)

# Authenticate a user
if service.authenticate("user1", "password1")
  puts "User authenticated successfully."
else
  puts "Invalid username or password."
end

# Create a new user account
service.create("user3", "password3")

# Delete a user account
service.delete("user2")
```

This code defines a module for managing user accounts, including classes for representing accounts, a repository for storing accounts, and a service for authenticating users and managing accounts. The usage example demonstrates how to use the service to authenticate users, create new accounts, and delete existing accounts.