```ruby
# Define a module for managing user accounts
module UserAccounts

  # Class to represent a user account
  class Account

    # Initialize a new user account
    def initialize(username, password)
      @username = username
      @password = password
      @balance = 0
    end

    # Deposit money into the account
    def deposit(amount)
      @balance += amount
    end

    # Withdraw money from the account
    def withdraw(amount)
      if amount <= @balance
        @balance -= amount
      else
        raise "Insufficient funds"
      end
    end

    # Get the account balance
    def balance
      @balance
    end

    # Check if the password is correct
    def authenticate(password)
      @password == password
    end
  end

  # Class to manage the user accounts
  class Manager

    # Initialize the user accounts manager
    def initialize
      @accounts = Hash.new
    end

    # Create a new user account
    def create_account(username, password)
      @accounts[username] = Account.new(username, password)
    end

    # Get an existing user account
    def get_account(username)
      @accounts[username]
    end
  end
end

# Create a new user accounts manager
manager = UserAccounts::Manager.new

# Create a new user account
manager.create_account("alice", "secret")

# Get the user account
account = manager.get_account("alice")

# Deposit money into the account
account.deposit(100)

# Withdraw money from the account
account.withdraw(50)

# Get the account balance
balance = account.balance

# Check if the password is correct
authenticated = account.authenticate("secret")

# Print the account balance and authentication status
puts "Account balance: #{balance}"
puts "Authentication status: #{authenticated}"
```

This code defines a module for managing user accounts. The module contains two classes: Account and Manager. The Account class represents a user account and has methods for depositing and withdrawing money, getting the account balance, and authenticating the user. The Manager class manages the user accounts and has methods for creating and getting user accounts.

The code then creates a new user accounts manager, creates a new user account, gets the user account, deposits money into the account, withdraws money from the account, gets the account balance, and checks if the password is correct. Finally, the code prints the account balance and authentication status.