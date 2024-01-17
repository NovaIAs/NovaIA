```objective-c
#import <Foundation/Foundation.h>

@interface ComplexClass : NSObject

// Define properties
@property (nonatomic, strong) NSString *name;
@property (nonatomic, assign) NSInteger age;
@property (nonatomic, strong) NSArray *hobbies;

// Initialize the class
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age hobbies:(NSArray *)hobbies;

// Define methods
- (void)sayHello;
- (void)printHobbies;

@end

@implementation ComplexClass

// Implement the initializer
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age hobbies:(NSArray *)hobbies {
    self = [super init];
    if (self) {
        self.name = name;
        self.age = age;
        self.hobbies = hobbies;
    }
    return self;
}

// Implement the methods
- (void)sayHello {
    NSLog(@"Hello, my name is %@ and I am %ld years old.", self.name, self.age);
}

- (void)printHobbies {
    NSLog(@"My hobbies are:");
    for (NSString *hobby in self.hobbies) {
        NSLog(@"- %@", hobby);
    }
}

@end

int main() {
    // Create an instance of the ComplexClass
    ComplexClass *person1 = [[ComplexClass alloc] initWithName:@"John Doe" age:30 hobbies:@[@"Hiking", @"Biking", @"Reading"]];

    // Call the methods on the instance
    [person1 sayHello];
    [person1 printHobbies];

    return 0;
}
```

Explanation:

1. We define a class called `ComplexClass` that inherits from `NSObject`.

2. We define properties for the class: `name`, `age`, and `hobbies`.

3. We implement an initializer method `initWithName:age:hobbies:` to initialize the properties of the class when an instance is created.

4. We define two methods: `sayHello` and `printHobbies`.

5. In the `main` function, we create an instance of the `ComplexClass` called `person1` using the initializer method and passing values for the properties.

6. We then call the `sayHello` and `printHobbies` methods on the `person1` instance to print out the name, age, and hobbies of the person.

This code demonstrates a more complex class with multiple properties, an initializer method, and multiple methods. It also shows how to create an instance of the class and call methods on it.