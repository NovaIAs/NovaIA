```objective-c
#import <Foundation/Foundation.h>

@interface ComplexClass : NSObject

- (instancetype)initWithName:(NSString *)name;
- (void)doSomething;

@end

@implementation ComplexClass

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
    }
    return self;
}

- (void)doSomething {
    NSLog(@"Hello, %@", _name);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        ComplexClass *object = [[ComplexClass alloc] initWithName:@"Bob"];
        [object doSomething];
    }
    return 0;
}
```

This is a simple Objective-C program that creates an instance of the `ComplexClass` class and calls the `doSomething` method on it.

The `ComplexClass` class has two properties: `name` and `doSomething`. The `name` property is a string that stores the name of the object. The `doSomething` property is a method that prints a greeting to the console.

The `main` function creates an instance of the `ComplexClass` class and calls the `doSomething` method on it. The `NSLog` function is used to print a message to the console.

This is just a simple example of an Objective-C program. Objective-C is a powerful language that can be used to create a wide variety of applications.