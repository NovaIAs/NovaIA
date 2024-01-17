```cool
-- Define a class Person with attributes name, age, and balance
class Person {
    name: String;
    age: Int;
    balance: Real;

    -- Constructor to initialize the attributes
    new(name: String, age: Int, balance: Real) {
        self.name := name;
        self.age := age;
        self.balance := balance;
    }

    -- Method to display the person's information
    display() {
        io.out_string(self.name);
        io.out_int(self.age);
        io.out_real(self.balance);
    }

    -- Method to deposit money into the person's account
    deposit(amount: Real) {
        self.balance := self.balance + amount;
    }

    -- Method to withdraw money from the person's account
    withdraw(amount: Real) {
        if (amount <= self.balance) {
            self.balance := self.balance - amount;
        } else {
            io.out_string("Insufficient funds");
        }
    }
}

-- Define a class BankAccount with attributes accountNumber and balance
class BankAccount {
    accountNumber: String;
    balance: Real;

    -- Constructor to initialize the attributes
    new(accountNumber: String, balance: Real) {
        self.accountNumber := accountNumber;
        self.balance := balance;
    }

    -- Method to display the bank account information
    display() {
        io.out_string(self.accountNumber);
        io.out_real(self.balance);
    }

    -- Method to deposit money into the bank account
    deposit(amount: Real) {
        self.balance := self.balance + amount;
    }

    -- Method to withdraw money from the bank account
    withdraw(amount: Real) {
        if (amount <= self.balance) {
            self.balance := self.balance - amount;
        } else {
            io.out_string("Insufficient funds");
        }
    }
}

-- Define a class Bank with attributes name and accounts
class Bank {
    name: String;
    accounts: Array[BankAccount];

    -- Constructor to initialize the attributes
    new(name: String) {
        self.name := name;
        self.accounts := Array.new[BankAccount]();
    }

    -- Method to add a bank account to the bank
    addAccount(account: BankAccount) {
        self.accounts.add(account);
    }

    -- Method to display the bank's information
    display() {
        io.out_string(self.name);
        io.out_int(self.accounts.length);
        for (i: Int in 0..self.accounts.length-1) {
            self.accounts[i].display();
        }
    }
}

-- Create a person object
person := Person.new("John Doe", 30, 1000.0);

-- Create a bank account object
account := BankAccount.new("1234567890", 500.0);

-- Create a bank object
bank := Bank.new("First National Bank");

-- Add the bank account to the bank
bank.addAccount(account);

-- Display the person's information
person.display();

-- Display the bank account information
account.display();

-- Display the bank's information
bank.display();
```

This code defines three classes: Person, BankAccount, and Bank. The Person class represents a person with attributes name, age, and balance. The BankAccount class represents a bank account with attributes accountNumber and balance. The Bank class represents a bank with attributes name and accounts.

The code also creates a person object, a bank account object, and a bank object. The person object is initialized with the name "John Doe", age 30, and balance 1000.0. The bank account object is initialized with the account number "1234567890" and balance 500.0. The bank object is initialized with the name "First National Bank".

The bank account object is then added to the bank object. The person's information, the bank account information, and the bank's information are then displayed.