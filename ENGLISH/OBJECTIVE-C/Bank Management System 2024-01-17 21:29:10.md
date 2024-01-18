```objective-c
#import <Foundation/Foundation.h>

// Define the `BankAccount` class.
@interface BankAccount : NSObject

// Declare the private instance variables.
@private
    NSString *accountNumber;
    NSString *accountHolderName;
    double balance;

// Declare the public methods.
@public
    - (instancetype)initWithAccountNumber:(NSString *)accountNumber
                          accountHolderName:(NSString *)accountHolderName;
    - (void)deposit:(double)amount;
    - (void)withdraw:(double)amount;
    - (double)getBalance;
    - (NSString *)getAccountNumber;
    - (NSString *)getAccountHolderName;

@end

// Implement the `BankAccount` class.
@implementation BankAccount

// Initialize the `BankAccount` object.
- (instancetype)initWithAccountNumber:(NSString *)accountNumber
                          accountHolderName:(NSString *)accountHolderName {
    self = [super init];
    if (self) {
        self->accountNumber = accountNumber;
        self->accountHolderName = accountHolderName;
        self->balance = 0.0;
    }
    return self;
}

// Deposit money into the account.
- (void)deposit:(double)amount {
    if (amount > 0) {
        self->balance += amount;
    }
}

// Withdraw money from the account.
- (void)withdraw:(double)amount {
    if (amount > 0 && amount <= self->balance) {
        self->balance -= amount;
    }
}

// Get the account balance.
- (double)getBalance {
    return self->balance;
}

// Get the account number.
- (NSString *)getAccountNumber {
    return self->accountNumber;
}

// Get the account holder name.
- (NSString *)getAccountHolderName {
    return self->accountHolderName;
}

@end

// Define the `Customer` class.
@interface Customer : NSObject

// Declare the private instance variables.
@private
    NSString *customerId;
    NSString *customerName;
    NSArray<BankAccount *> *bankAccounts;

// Declare the public methods.
@public
    - (instancetype)initWithCustomerId:(NSString *)customerId
                          customerName:(NSString *)customerName;
    - (void)addBankAccount:(BankAccount *)bankAccount;
    - (NSArray<BankAccount *> *)getBankAccounts;
    - (NSString *)getCustomerId;
    - (NSString *)getCustomerName;

@end

// Implement the `Customer` class.
@implementation Customer

// Initialize the `Customer` object.
- (instancetype)initWithCustomerId:(NSString *)customerId
                          customerName:(NSString *)customerName {
    self = [super init];
    if (self) {
        self->customerId = customerId;
        self->customerName = customerName;
        self->bankAccounts = @[];
    }
    return self;
}

// Add a bank account to the customer.
- (void)addBankAccount:(BankAccount *)bankAccount {
    self->bankAccounts = [self->bankAccounts arrayByAddingObject:bankAccount];
}

// Get the customer's bank accounts.
- (NSArray<BankAccount *> *)getBankAccounts {
    return self->bankAccounts;
}

// Get the customer ID.
- (NSString *)getCustomerId {
    return self->customerId;
}

// Get the customer name.
- (NSString *)getCustomerName {
    return self->customerName;
}

@end

// Define the `Bank` class.
@interface Bank : NSObject

// Declare the private instance variables.
@private
    NSString *bankName;
    NSArray<Customer *> *customers;

// Declare the public methods.
@public
    - (instancetype)initWithBankName:(NSString *)bankName;
    - (void)addCustomer:(Customer *)customer;
    - (NSArray<Customer *> *)getCustomers;
    - (NSString *)getBankName;

@end

// Implement the `Bank` class.
@implementation Bank

// Initialize the `Bank` object.
- (instancetype)initWithBankName:(NSString *)bankName {
    self = [super init];
    if (self) {
        self->bankName = bankName;
        self->customers = @[];
    }
    return self;
}

// Add a customer to the bank.
- (void)addCustomer:(Customer *)customer {
    self->customers = [self->customers arrayByAddingObject:customer];
}

// Get the bank's customers.
- (NSArray<Customer *> *)getCustomers {
    return self->customers;
}

// Get the bank name.
- (NSString *)getBankName {
    return self->bankName;
}

@end

// Main function.
int main() {
    // Create a bank.
    Bank *bank = [[Bank alloc] initWithBankName:@"XYZ Bank"];

    // Create a customer.
    Customer *customer1 = [[Customer alloc] initWithCustomerId:@"C001"
                                                   customerName:@"John Doe"];

    // Create a bank account for the customer.
    BankAccount *bankAccount1 = [[BankAccount alloc] initWithAccountNumber:@"A001"
                                                         accountHolderName:@"John Doe"];

    // Add the bank account to the customer.
    [customer1 addBankAccount:bankAccount1];

    // Add the customer to the bank.
    [bank addCustomer:customer1];

    // Create another customer.
    Customer *customer2 = [[Customer alloc] initWithCustomerId:@"C002"
                                                   customerName:@"Jane Smith"];

    // Create a bank account for the customer.
    BankAccount *bankAccount2 = [[BankAccount alloc] initWithAccountNumber:@"A002"
                                                         accountHolderName:@"Jane Smith"];

    // Add the bank account to the customer.
    [customer2 addBankAccount:bankAccount2];

    // Add the customer to the bank.
    [bank addCustomer:customer2];

    // Get all customers from the bank.
    NSArray<Customer *> *customers = [bank getCustomers];

    // Iterate over the customers and print their details.
    for (Customer *customer in customers) {
        NSLog(@"Customer ID: %@", [customer getCustomerId]);
        NSLog(@"Customer Name: %@", [customer getCustomerName]);

        // Get all bank accounts for the customer.
        NSArray<BankAccount *> *bankAccounts = [customer getBankAccounts];

        // Iterate over the bank accounts and print their details.
        for (BankAccount *bankAccount in bankAccounts) {
            NSLog(@"Account Number: %@", [bankAccount getAccountNumber]);
            NSLog(@"Account Holder Name: %@", [bankAccount getAccountHolderName]);
            NSLog(@"Balance: %f", [bankAccount getBalance]);
        }

        NSLog(@"\n");
    }

    return 0;
}
```

Explanation:

1. We define three classes: `BankAccount`, `Customer`, and `Bank`.

2. The `BankAccount` class represents a bank account with an account number, account holder name, and balance. It provides methods to deposit and withdraw money, get the balance, and get the account number and holder name.

3. The `Customer` class represents a customer of the bank. It has a customer ID, customer name, and a list of bank accounts. It provides methods to add bank accounts, get all bank accounts, and get the customer ID and name.

4. The `Bank` class represents a bank. It has a bank name and a list of customers. It provides methods to add customers, get all customers, and get the bank name.

5. In the `main` function, we create a bank, two customers, and two bank accounts. We add the bank accounts to the customers and add the customers to the bank.

6. We iterate over the customers, print their details, and iterate over their bank accounts to print their details as well.

This code demonstrates the use of classes, objects, arrays, loops, and methods in Objective-C to create a complex and differentiated banking application.