```cool
// In this program we will create a bank account class that allows users to deposit and withdraw money, and also to earn interest on their deposits.

class BankAccount {
    balance : Int;   // The current balance in the account.
    interestRate : Float;  // The annual interest rate on the account.

    constructor(initialBalance : Int, initialInterestRate : Float) {
        balance := initialBalance;
        interestRate := initialInterestRate;
    }

    deposit(amount : Int) {
        balance := balance + amount;
    }

    withdraw(amount : Int) {
        if amount <= balance then
            balance := balance - amount;
        else
            error("Insufficient funds.");
        fi;
    }

    earnInterest() {
        balance := balance * (1 + interestRate);
    }
}

// Create a new bank account with an initial balance of $1000 and an annual interest rate of 5%.
account := new BankAccount(1000, 0.05);

// Deposit $500 into the account.
account.deposit(500);

// Withdraw $300 from the account.
account.withdraw(300);

// Earn interest on the account for one year.
account.earnInterest();

// Print the final balance of the account.
print(account.balance);  // Output: 1225

```

This code defines a class called BankAccount that has two fields: `balance` and `interestRate`. The `balance` field stores the current balance in the account, and the `interestRate` field stores the annual interest rate on the account.

The class also has three methods: `deposit`, `withdraw`, and `earnInterest`. The `deposit` method allows the user to deposit money into the account, the `withdraw` method allows the user to withdraw money from the account, and the `earnInterest` method allows the user to earn interest on their deposits.

To use the BankAccount class, we first create a new instance of the class using the `new` keyword. We then call the `deposit`, `withdraw`, and `earnInterest` methods on the object to perform the desired operations on the account.

For example, in the code above, we first create a new bank account with an initial balance of $1000 and an annual interest rate of 5%. We then deposit $500 into the account, withdraw $300 from the account, and earn interest on the account for one year. Finally, we print the final balance of the account, which is $1225.