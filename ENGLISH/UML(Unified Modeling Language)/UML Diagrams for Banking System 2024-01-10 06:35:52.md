```
+-----------------------------------------------------------------------------------+
|                                  Class Diagram                                    |
+-----------------------------------------------------------------------------------+

+----------------+                                      +--------------------+
|                |                                      |                    |
|  AccountOwner  |                                      |  BankAccount      |
|                |                                      |                    |
+----------------+                                      +--------------------+
| - id: String   |                                      | - id: String       |
| - name: String |                                      | - balance: Double  |
| - email: String|                                      | - type: String    |
|                |                                      | - ownerId: String  |
+----------------+                                      +--------------------+

+----------------+                                      +-------------------+
|                                  |                                  |
|  Transaction   |                                  |  Bank           |
|                                  |                                  |
+----------------+                                  +-------------------+
| - id: String   |                                  | - id: String       |
| - amount: Double|                                  | - name: String     |
| - type: String |                                  | - address: String  |
| - accountId: String|                                  | - phone: String    |
| - date: Date    |                                  | - accounts: List   |
+----------------+                                  +-------------------+

+-----------------------------------------------------------------------------------+
|                                Sequence Diagram                                     |
+-----------------------------------------------------------------------------------+

AccountOwner -> Bank: createAccount(type)
Bank -> BankAccount: new(type, ownerId)
Bank -> AccountOwner: getAccount(id)

AccountOwner -> BankAccount: deposit(amount)
BankAccount -> BankAccount: addBalance(amount)

AccountOwner -> BankAccount: withdraw(amount)
BankAccount -> BankAccount: subtractBalance(amount)

AccountOwner -> BankAccount: transfer(amount, toAccountId)
BankAccount -> BankAccount: subtractBalance(amount)
BankAccount -> BankAccount: addBalance(amount)

+-----------------------------------------------------------------------------------+
|                                 Activity Diagram                                  |
+-----------------------------------------------------------------------------------+

[Start]
AccountOwner -> Bank: createAccount(type)
Bank -> BankAccount: new(type, ownerId)
Bank -> AccountOwner: getAccount(id)

[Loop]
AccountOwner -> BankAccount: deposit(amount) or withdraw(amount) or transfer(amount, toAccountId)
BankAccount -> BankAccount: addBalance(amount) or subtractBalance(amount)

[End]

+-----------------------------------------------------------------------------------+
|                                  Use Case Diagram                                   |
+-----------------------------------------------------------------------------------+

AccountOwner -> Bank: Create Account
AccountOwner -> BankAccount: Deposit Money
AccountOwner -> BankAccount: Withdraw Money
AccountOwner -> BankAccount: Transfer Money

+-----------------------------------------------------------------------------------+
|                                  Class Explanation                                 |
+-----------------------------------------------------------------------------------+

- AccountOwner: Represents a customer who has an account with the bank.
- BankAccount: Represents a bank account that belongs to an account owner.
- Transaction: Represents a transaction that has been made on a bank account.
- Bank: Represents the bank that provides banking services to account owners.

+-----------------------------------------------------------------------------------+
|                             Sequence Diagram Explanation                              |
+-----------------------------------------------------------------------------------+

- The sequence diagram shows the steps involved in a user creating an account, depositing money, withdrawing money, and transferring money.
- The user first creates an account with the bank.
- The bank creates a new bank account for the user.
- The user then deposits money into their account.
- The bank adds the money to the user's account balance.
- The user then withdraws money from their account.
- The bank subtracts the money from the user's account balance.
- The user then transfers money from their account to another account.
- The bank subtracts the money from the user's account balance and adds it to the recipient's account balance.

+-----------------------------------------------------------------------------------+
|                            Activity Diagram Explanation                             |
+-----------------------------------------------------------------------------------+

- The activity diagram shows the flow of activities that occur when a user interacts with their bank account.
- The user can deposit money, withdraw money, or transfer money.
- The bank processes the transaction and updates the user's account balance.

+-----------------------------------------------------------------------------------+
|                           Use Case Diagram Explanation                            |
+-----------------------------------------------------------------------------------+

- The use case diagram shows the different use cases that are available to users of the banking system.
- The use cases are: Create Account, Deposit Money, Withdraw Money, and Transfer Money.

```