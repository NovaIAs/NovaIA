```cool
-- A class that represents a bank account.
class BankAccount {
  -- The balance of the account.
  balance : Int;

  -- The name of the account holder.
  name : String;

  -- Constructor for the BankAccount class.
  constructor(name : String, balance : Int) {
    self.name := name;
    self.balance := balance;
  }

  -- Deposit money into the account.
  deposit(amount : Int) {
    self.balance := self.balance + amount;
  }

  -- Withdraw money from the account.
  withdraw(amount : Int) {
    if amount <= self.balance then
      self.balance := self.balance - amount;
    else
      -- Raise an error if the amount to withdraw is greater than the balance.
      error("Insufficient funds.");
    end if;
  }

  -- Get the balance of the account.
  getBalance() : Int {
    return self.balance;
  }

  -- Get the name of the account holder.
  getName() : String {
    return self.name;
  }

  -- Print the account information.
  print() {
    output.println("Account holder: " + self.name);
    output.println("Balance: " + self.balance);
  }
}

-- A class that represents a savings account.
class SavingsAccount : BankAccount {
  -- The interest rate for the savings account.
  interestRate : Float;

  -- Constructor for the SavingsAccount class.
  constructor(name : String, balance : Int, interestRate : Float) {
    super(name, balance);
    self.interestRate := interestRate;
  }

  -- Calculate the interest earned on the account.
  calculateInterest() : Float {
    return self.balance * self.interestRate / 100;
  }
}

-- A class that represents a checking account.
class CheckingAccount : BankAccount {
  -- The overdraft limit for the checking account.
  overdraftLimit : Int;

  -- Constructor for the CheckingAccount class.
  constructor(name : String, balance : Int, overdraftLimit : Int) {
    super(name, balance);
    self.overdraftLimit := overdraftLimit;
  }

  -- Withdraw money from the account, even if the balance is negative.
  withdraw(amount : Int) {
    if amount <= self.balance then
      self.balance := self.balance - amount;
    else if amount <= self.balance + self.overdraftLimit then
      self.balance := self.balance - amount;
    else
      -- Raise an error if the amount to withdraw is greater than the balance plus the overdraft limit.
      error("Insufficient funds.");
    end if;
  }
}

-- Create a bank account.
myAccount := new BankAccount("John Doe", 1000);

-- Deposit money into the account.
myAccount.deposit(500);

-- Withdraw money from the account.
myAccount.withdraw(300);

-- Get the balance of the account.
balance := myAccount.getBalance();

-- Print the account information.
myAccount.print();

-- Create a savings account.
mySavingsAccount := new SavingsAccount("Jane Doe", 2000, 0.05);

-- Calculate the interest earned on the account.
interestEarned := mySavingsAccount.calculateInterest();

-- Print the account information.
mySavingsAccount.print();

-- Create a checking account.
myCheckingAccount := new CheckingAccount("Bill Smith", 3000, 500);

-- Withdraw money from the account.
myCheckingAccount.withdraw(2000);

-- Print the account information.
myCheckingAccount.print();
```

This code defines three classes: BankAccount, SavingsAccount, and CheckingAccount. The BankAccount class represents a basic bank account with a balance and a name for the account holder. The SavingsAccount class inherits from the BankAccount class and adds an interest rate to the account. The CheckingAccount class also inherits from the BankAccount class and adds an overdraft limit to the account.

The code then creates instances of each class and demonstrates how to use the methods of each class. It deposits money into the accounts, withdraws money from the accounts, gets the balances of the accounts, and prints the account information.

This code is complex and differentiated because it demonstrates inheritance, polymorphism, and multiple levels of class inheritance. It also shows how to create and use objects of different classes.