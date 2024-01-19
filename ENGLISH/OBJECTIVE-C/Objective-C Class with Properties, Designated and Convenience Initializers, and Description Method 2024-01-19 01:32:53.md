```objective-c
#import <Foundation/Foundation.h>

@interface MyClass : NSObject {
    // Instance variables
    NSString *name;
    int age;
}

// Declare properties
@property (nonatomic, strong) NSString *name;
@property (nonatomic) int age;

// Designated initializer
- (instancetype)initWithName:(NSString *)name andAge:(int)age;

// Convenience initializer
- (instancetype)initWithName:(NSString *)name;

// Other methods
- (NSString *)description;

@end

@implementation MyClass

// Designated initializer implementation
- (instancetype)initWithName:(NSString *)name andAge:(int)age {
    if (self = [super init]) {
        self.name = name;
        self.age = age;
    }
    return self;
}

// Convenience initializer implementation
- (instancetype)initWithName:(NSString *)name {
    return [self initWithName:name andAge:0];
}

// Description method implementation
- (NSString *)description {
    return [NSString stringWithFormat:@"My name is %@ and I am %d years old.", self.name, self.age];
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an instance of MyClass using the designated initializer
        MyClass *person1 = [[MyClass alloc] initWithName:@"John" andAge:25];
        
        // Create an instance of MyClass using the convenience initializer
        MyClass *person2 = [[MyClass alloc] initWithName:@"Jane"];
        
        // Print the descriptions of both instances
        NSLog(@"%@", person1);
        NSLog(@"%@", person2);
    }
    return 0;
}
```

This code demonstrates the use of Objective-C classes, properties, designated initializers, convenience initializers, and the description method. Here's an explanation of the code:

- We declare a class called MyClass that inherits from NSObject, which is the base class for all Objective-C objects.

- Inside the MyClass interface, we define two instance variables: `name` (which is a string) and `age` (which is an integer).

- We declare properties for both instance variables using the @property directive. Properties provide a way to access and modify instance variables in a safe and controlled manner.

- We define two initializers: a designated initializer (`initWithName:andAge:`) and a convenience initializer (`initWithName:`). Initializers are used to create instances of a class. The designated initializer takes two parameters: `name` and `age`, and initializes both instance variables. The convenience initializer takes only the `name` parameter and calls the designated initializer with a default age of 0.

- We implement the description method, which is used to return a string representation of an object. In this case, it returns a string containing the name and age of the person.

- In the main function, we create two instances of MyClass: `person1` using the designated initializer and `person2` using the convenience initializer.

- Finally, we print the descriptions of both instances using the NSLog function.

This code demonstrates the use of Objective-C classes, properties, initializers, and the description method. It allows you to create and initialize instances of a class and access their properties.