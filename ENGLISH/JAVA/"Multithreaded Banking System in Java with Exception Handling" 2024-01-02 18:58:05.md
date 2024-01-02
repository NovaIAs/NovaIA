Sure! Here's a complex and differentiated Java code that showcases various concepts, including object-oriented programming, multithreading, file handling, and exception handling. The code simulates a simple banking system with multiple accounts. Let's dive into the code:

```java
import java.io.*;
import java.util.*;

class BankAccount {
    private String accountNumber;
    private double balance;
    
    public BankAccount(String accountNumber, double initialBalance) {
        this.accountNumber = accountNumber;
        this.balance = initialBalance;
    }
    
    public void deposit(double amount) {
        balance += amount;
        System.out.println("Deposit of $" + amount + " successful. New balance: $" + balance);
    }
    
    public void withdraw(double amount) throws InsufficientFundsException {
        if (balance >= amount) {
            balance -= amount;
            System.out.println("Withdrawal of $" + amount + " successful. New balance: $" + balance);
        } else {
            throw new InsufficientFundsException("Insufficient funds in the account.");
        }
    }
    
    public double getBalance() {
        return balance;
    }
    
    public String getAccountNumber() {
        return accountNumber;
    }
}

class InsufficientFundsException extends Exception {
    public InsufficientFundsException(String message) {
        super(message);
    }
}

public class BankSystem {
    private static List<BankAccount> accounts = new ArrayList<>();
    
    public static void main(String[] args) {
        loadAccountsFromFile("accounts.txt");
        
        Thread depositThread = new Thread(() -> {
            try {
                depositToAccounts();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });
        
        Thread withdrawalThread = new Thread(() -> {
            try {
                withdrawFromAccounts();
            } catch (InterruptedException | InsufficientFundsException e) {
                e.printStackTrace();
            }
        });
        
        depositThread.start();
        withdrawalThread.start();
        
        try {
            depositThread.join();
            withdrawalThread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        
        saveAccountsToFile("updated_accounts.txt");
    }
    
    private static void loadAccountsFromFile(String filename) {
        try (Scanner scanner = new Scanner(new File(filename))) {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] accountData = line.split(",");
                String accountNumber = accountData[0];
                double balance = Double.parseDouble(accountData[1]);
                BankAccount account = new BankAccount(accountNumber, balance);
                accounts.add(account);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
    
    private static void depositToAccounts() throws InterruptedException {
        Random random = new Random();
        for (BankAccount account : accounts) {
            double amount = random.nextDouble() * 1000;
            account.deposit(amount);
            Thread.sleep(500);
        }
    }
    
    private static void withdrawFromAccounts() throws InterruptedException, InsufficientFundsException {
        Random random = new Random();
        for (BankAccount account : accounts) {
            double amount = random.nextDouble() * 500;
            try {
                account.withdraw(amount);
            } catch (InsufficientFundsException e) {
                System.out.println("Error: " + e.getMessage() + " Account Number: " + account.getAccountNumber());
            }
            Thread.sleep(500);
        }
    }
    
    private static void saveAccountsToFile(String filename) {
        try (PrintWriter writer = new PrintWriter(new File(filename))) {
            for (BankAccount account : accounts) {
                writer.println(account.getAccountNumber() + "," + account.getBalance());
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
```

Explanation:
- The code starts by defining a `BankAccount` class that represents a bank account. It has fields for account number and balance.
- The class provides methods for depositing and withdrawing funds, as well as accessing the balance and account number.
- The `withdraw` method throws a custom `InsufficientFundsException` if the withdrawal amount exceeds the account balance.
- The `BankSystem` class represents the main entry point of the program.
- It contains a list of `BankAccount` objects to store multiple accounts.
- The `main` method loads account data from a file using the `loadAccountsFromFile` method.
- Two threads, `depositThread` and `withdrawalThread`, are created to simulate concurrent deposit and withdrawal operations.
- The `depositToAccounts` method generates random deposit amounts and calls the `deposit` method on each account.
- The `withdrawFromAccounts` method generates random withdrawal amounts and calls the `withdraw` method on each account.
- Exception handling is implemented to catch `InterruptedException` and `InsufficientFundsException`.
- After the threads complete, the `saveAccountsToFile` method is called to save the updated account data to a file.

Please note that this is just one example of a complex Java code. The possibilities are endless, and you can create even more intricate programs based on your requirements.