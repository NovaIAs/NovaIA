```objective-c
#import <Foundation/Foundation.h>

@interface Employee : NSObject

@property (nonatomic, strong) NSString *name;
@property (nonatomic, assign) NSInteger age;
@property (nonatomic, assign) double salary;

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age salary:(double)salary;

@end

@implementation Employee

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age salary:(double)salary {
    self = [super init];
    if (self) {
        _name = name;
        _age = age;
        _salary = salary;
    }
    return self;
}

@end

@interface Company : NSObject

@property (nonatomic, strong) NSString *name;
@property (nonatomic, strong) NSArray<Employee *> *employees;

- (instancetype)initWithName:(NSString *)name employees:(NSArray<Employee *> *)employees;

@end

@implementation Company

- (instancetype)initWithName:(NSString *)name employees:(NSArray<Employee *> *)employees {
    self = [super init];
    if (self) {
        _name = name;
        _employees = employees;
    }
    return self;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create some employees
        Employee *employee1 = [[Employee alloc] initWithName:@"John Doe" age:30 salary:40000];
        Employee *employee2 = [[Employee alloc] initWithName:@"Jane Smith" age:25 salary:50000];
        Employee *employee3 = [[Employee alloc] initWithName:@"Bill Jones" age:40 salary:60000];

        // Create a company
        Company *company = [[Company alloc] initWithName:@"Acme Corporation" employees:@[employee1, employee2, employee3]];

        // Print the company's name and employees
        NSLog(@"Company name: %@", company.name);
        for (Employee *employee in company.employees) {
            NSLog(@"Employee name: %@, age: %ld, salary: %.2f", employee.name, employee.age, employee.salary);
        }
    }
    return 0;
}
```

This code defines a class called `Employee` with properties for `name`, `age`, and `salary`. It also defines a `Company` class with properties for `name` and `employees`, where `employees` is an array of `Employee` objects. The `main` function creates three `Employee` objects, adds them to a `Company` object, and then prints the company's name and employees.

Here is a more detailed explanation of the code:

* The `Employee` class has three instance variables: `name`, `age`, and `salary`. These variables are declared as properties using the `@property` directive. The `@synthesize` directive tells the compiler to generate the getter and setter methods for these properties.
* The `Employee` class has an initializer method that takes three parameters: `name`, `age`, and `salary`. This method initializes the instance variables with the values of the parameters.
* The `Company` class has two instance variables: `name` and `employees`. The `name` variable is a string, and the `employees` variable is an array of `Employee` objects.
* The `Company` class has an initializer method that takes two parameters: `name` and `employees`. This method initializes the instance variables with the values of the parameters.
* The `main` function creates three `Employee` objects using the `[[Employee alloc] initWithName:age:salary:]` method. These objects are added to an array, which is then passed to the `[[Company alloc] initWithName:employees:]` method to create a `Company` object.
* The `NSLog` function is used to print the company's name and employees. The `%@` format specifier is used to print the value of an `NSString` object, and the `%ld` and `%.2f` format specifiers are used to print the values of `NSInteger` and `double` objects, respectively.

This code demonstrates how to define and use classes and objects in Objective-C. It also shows how to use the `NSLog` function to print values.