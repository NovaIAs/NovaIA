Certainly! Here's a complex and differentiated code written in the MODULA-2 programming language. This code implements a simulated banking system with support for multiple accounts, transactions, and balance management. 

```modula2
MODULE BankingSystem;

FROM InOut IMPORT WriteString, WriteLn;
FROM SYSTEM IMPORT ADR;

CONST
    MAX_ACCOUNTS = 100;
    MAX_TRANSACTIONS = 1000;

TYPE
    AccountID = CARDINAL;
    TransactionID = CARDINAL;

    AccountRecord = RECORD
        id: AccountID;
        balance: REAL;
    END;

    TransactionRecord = RECORD
        id: TransactionID;
        accountID: AccountID;
        amount: REAL;
        description: ARRAY [0..255] OF CHAR;
    END;

VAR
    accounts: ARRAY [1..MAX_ACCOUNTS] OF AccountRecord;
    transactions: ARRAY [1..MAX_TRANSACTIONS] OF TransactionRecord;
    numAccounts: CARDINAL := 0;
    numTransactions: CARDINAL := 0;

PROCEDURE CreateAccount(balance: REAL): AccountID;
    VAR
        account: AccountRecord;
    BEGIN
        IF numAccounts < MAX_ACCOUNTS THEN
            account.id := numAccounts + 1;
            account.balance := balance;
            accounts[account.id] := account;
            numAccounts := numAccounts + 1;
            RETURN account.id;
        ELSE
            RETURN 0;  (* Error: Maximum number of accounts reached *)
        END;
    END CreateAccount;

PROCEDURE MakeTransaction(accountID: AccountID; amount: REAL; description: ARRAY OF CHAR): TransactionID;
    VAR
        transaction: TransactionRecord;
        account: AccountRecord;
    BEGIN
        IF (accountID > 0) AND (accountID <= numAccounts) THEN
            IF accounts[accountID].balance + amount >= 0 THEN
                transaction.id := numTransactions + 1;
                transaction.accountID := accountID;
                transaction.amount := amount;
                transaction.description := description;
                transactions[transaction.id] := transaction;

                account := accounts[accountID];
                account.balance := account.balance + amount;
                accounts[accountID] := account;

                numTransactions := numTransactions + 1;
                RETURN transaction.id;
            ELSE
                RETURN 0;  (* Error: Insufficient funds *)
            END;
        ELSE
            RETURN 0;  (* Error: Invalid account ID *)
        END;
    END MakeTransaction;

PROCEDURE GetAccountBalance(accountID: AccountID): REAL;
    BEGIN
        IF (accountID > 0) AND (accountID <= numAccounts) THEN
            RETURN accounts[accountID].balance;
        ELSE
            RETURN 0;  (* Error: Invalid account ID *)
        END;
    END GetAccountBalance;

PROCEDURE GetTransactionDescription(transactionID: TransactionID): ARRAY OF CHAR;
    BEGIN
        IF (transactionID > 0) AND (transactionID <= numTransactions) THEN
            RETURN transactions[transactionID].description;
        ELSE
            RETURN '';  (* Error: Invalid transaction ID *)
        END;
    END GetTransactionDescription;

BEGIN
    (* The main program can use the provided procedures to interact with the banking system *)
END BankingSystem.
```

This code defines a module named `BankingSystem` that contains procedures for creating accounts, making transactions, retrieving account balances, and getting transaction descriptions. The module uses two record types, `AccountRecord` and `TransactionRecord`, to store account and transaction information, respectively.

The `CreateAccount` procedure creates a new account with the specified initial balance. It returns the ID of the created account or 0 if the maximum number of accounts has been reached.

The `MakeTransaction` procedure allows making a transaction for a given account ID. It checks if the account exists and if the transaction would result in a non-negative balance. If successful, it returns the ID of the created transaction; otherwise, it returns 0.

The `GetAccountBalance` procedure retrieves the current balance of a given account ID. It returns the balance or 0 if the account ID is invalid.

The `GetTransactionDescription` procedure retrieves the description of a given transaction ID. It returns the description or an empty string if the transaction ID is invalid.

The main program can use these procedures to interact with the banking system. You can add additional functionality as needed.