```ruby
# This program simulates a simple banking system.

# Create a class called BankAccount to represent a bank account.
class BankAccount
  # Initialize a bank account with a balance and an account number.
  def initialize(balance, account_number)
    @balance = balance
    @account_number = account_number
  end

  # Deposit money into the account.
  def deposit(amount)
    @balance += amount
  end

  # Withdraw money from the account.
  def withdraw(amount)
    if amount <= @balance
      @balance -= amount
    else
      raise "Insufficient funds"
    end
  end

  # Get the balance of the account.
  def balance
    @balance
  end

  # Get the account number.
  def account_number
    @account_number
  end
end

# Create a class called Bank to represent a bank.
class Bank
  # Initialize a bank with a list of bank accounts.
  def initialize(accounts)
    @accounts = accounts
  end

  # Add a new account to the bank.
  def add_account(account)
    @accounts << account
  end

  # Get all the accounts in the bank.
  def accounts
    @accounts
  end

  # Process a transaction.
  def process_transaction(transaction)
    # Check if the transaction is valid.
    if transaction.valid?
      # Get the accounts involved in the transaction.
      from_account = @accounts.find { |account| account.account_number == transaction.from_account }
      to_account = @accounts.find { |account| account.account_number == transaction.to_account }

      # Perform the transaction.
      from_account.withdraw(transaction.amount)
      to_account.deposit(transaction.amount)
    else
      raise "Invalid transaction"
    end
  end
end

# Create a class called Transaction to represent a transaction.
class Transaction
  # Initialize a transaction with a from account, a to account, and an amount.
  def initialize(from_account, to_account, amount)
    @from_account = from_account
    @to_account = to_account
    @amount = amount
  end

  # Check if the transaction is valid.
  def valid?
    @from_account && @to_account && @amount > 0
  end

  # Get the from account.
  def from_account
    @from_account
  end

  # Get the to account.
  def to_account
    @to_account
  end

  # Get the amount.
  def amount
    @amount
  end
end

# Create a bank with some accounts.
bank = Bank.new([
  BankAccount.new(100, "123456789"),
  BankAccount.new(200, "987654321")
])

# Process a transaction.
bank.process_transaction(Transaction.new("123456789", "987654321", 50))

# Get the balance of the accounts.
puts bank.accounts[0].balance # 50
puts bank.accounts[1].balance # 250
```

This code simulates a simple banking system. It creates a Bank class that represents a bank, a BankAccount class that represents a bank account, and a Transaction class that represents a transaction. The Bank class has a list of bank accounts and can process transactions. The BankAccount class has a balance and an account number. The Transaction class has a from account, a to account, and an amount.

The program first creates a bank with two accounts. Then, it processes a transaction that transfers $50 from the first account to the second account. Finally, it prints the balances of the two accounts.