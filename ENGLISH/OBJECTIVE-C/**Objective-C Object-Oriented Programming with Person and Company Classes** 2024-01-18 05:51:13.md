```objective-c
#import <Foundation/Foundation.h>

@interface Person : NSObject

@property (nonatomic, strong) NSString *name;
@property (nonatomic, assign) NSInteger age;

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age;

@end

@implementation Person

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age {
    self = [super init];
    if (self) {
        self.name = name;
        self.age = age;
    }
    return self;
}

@end

@interface Company : NSObject

@property (nonatomic, strong) NSString *name;
@property (nonatomic, strong) NSArray<Person *> *employees;

- (instancetype)initWithName:(NSString *)name employees:(NSArray<Person *> *)employees;

@end

@implementation Company

- (instancetype)initWithName:(NSString *)name employees:(NSArray<Person *> *)employees {
    self = [super init];
    if (self) {
        self.name = name;
        self.employees = employees;
    }
    return self;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create some people
        Person *person1 = [[Person alloc] initWithName:@"John Doe" age:30];
        Person *person2 = [[Person alloc] initWithName:@"Jane Smith" age:25];
        Person *person3 = [[Person alloc] initWithName:@"Bill Jones" age:40];

        // Create a company
        Company *company = [[Company alloc] initWithName:@"Acme Corporation" employees:@[person1, person2, person3]];

        // Print the company's name and employees
        NSLog(@"Company: %@", company.name);
        for (Person *employee in company.employees) {
            NSLog(@"Employee: %@, Age: %ld", employee.name, employee.age);
        }
    }
    return 0;
}
```

Explanation:

1. We define a `Person` class with properties for name and age, and a method to initialize a person with a name and age.

2. We define a `Company` class with properties for name and employees, and a method to initialize a company with a name and a list of employees.

3. In the `main` function, we create three `Person` objects and one `Company` object.

4. We print the company's name and the names and ages of its employees.

This code demonstrates object-oriented programming in Objective-C, including creating and initializing objects, setting and getting properties, and using arrays to store objects.