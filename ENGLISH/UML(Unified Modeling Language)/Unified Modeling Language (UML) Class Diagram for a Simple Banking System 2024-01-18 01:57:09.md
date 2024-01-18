```
+--------------------------------+                                  +-----------+
|                                |                                  |           |
|  +--------------+              |              +--------------+ |           |
|  |  Account     |              |              |  Customer   | |           |
|  |--------------|              |              |--------------| |           |
|  | +id          |              |              | +id          | |           |
|  | +name        |              |              | +name        | |           |
|  | +balance     |              |              | +address     | |           |
|  | +pin         |              |              | +phone       | |           |
|  +--------------+              |              +--------------+ |           |
|                                |                                  |           |
|  +-------------+                +---------------+                  |           |
|  |  Transaction |                |  Withdrawal   |                  |           |
|  |-------------|                |---------------|                  |           |
|  | +id          |                | +id          |                  |           |
|  | +amount      |                | +amount      |                  |           |
|  | +date        |                | +date        |                  |           |
|  | +type        |                | +type        |                  |           |
|  | +account_id  |                | +account_id  |                  |           |
|  +-------------+                +---------------+                  |           |
|                                |                                  |           |
+--------------------------------+                                  +-----------+

+------------------------+                                   +-------------+
|                        |                                   |             |
|    +-----------------+  +---------------------------+   |             |
|    |  Bank Employee  |  |                           |   |             |
|    |-----------------|  |  +---------------------+  |   |             |
|    | +id            |  |  |  ATM Machine        |  |   |             |
|    | +name          |  |  |  +id              |  |   |             |
|    | +address       |  |  |  +location         |  |   |             |
|    | +phone         |  |  |  +cash_dispenser  |  |   |             |
|    | +salary        |  |  |  +card_reader     |  |   |             |
|    +-----------------+  |  |  +deposit_slot    |  |   |             |
|                        |  |  +---------------------+  |   |             |
|                        |  |                           |   |             |
|                        |  +---------------------------+   |             |
|                        |                                   |             |
+------------------------+                                   +-------------+
```

This UML class diagram represents a simple banking system. It includes the following classes:

* **Account:** Represents a bank account. It has the following attributes:
    * `id`: The unique identifier of the account.
    * `name`: The name of the account holder.
    * `balance`: The current balance of the account.
    * `pin`: The PIN number of the account.

* **Customer:** Represents a customer of the bank. It has the following attributes:
    * `id`: The unique identifier of the customer.
    * `name`: The name of the customer.
    * `address`: The address of the customer.
    * `phone`: The phone number of the customer.

* **Transaction:** Represents a transaction that has been made on an account. It has the following attributes:
    * `id`: The unique identifier of the transaction.
    * `amount`: The amount of money that was transferred in the transaction.
    * `date`: The date when the transaction took place.
    * `type`: The type of transaction that was made (e.g., "deposit", "withdrawal").
    * `account_id`: The ID of the account that the transaction was made on.

* **Bank Employee:** Represents an employee of the bank. It has the following attributes:
    * `id`: The unique identifier of the employee.
    * `name`: The name of the employee.
    * `address`: The address of the employee.
    * `phone`: The phone number of the employee.
    * `salary`: The salary of the employee.

* **ATM Machine:** Represents an ATM machine. It has the following attributes:
    * `id`: The unique identifier of the ATM machine.
    * `location`: The location of the ATM machine.
    * `cash_dispenser`: The cash dispenser of the ATM machine.
    * `card_reader`: The card reader of the ATM machine.
    * `deposit_slot`: The deposit slot of the ATM machine.

The relationships between the classes are as follows:

* An `Account` can have many `Transactions`.
* A `Customer` can have many `Accounts`.
* A `Bank Employee` can make `Transactions` on `Accounts`.
* An `ATM Machine` can be used to make `Transactions` on `Accounts`.