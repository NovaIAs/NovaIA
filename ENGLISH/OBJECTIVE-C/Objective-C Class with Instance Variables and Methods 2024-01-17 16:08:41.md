```objective-c
#import <Foundation/Foundation.h>

@interface MyClass : NSObject {
    int _instanceVariable;
}

- (void)setInstanceVariable:(int)value;
- (int)instanceVariable;

@end

@implementation MyClass

- (void)setInstanceVariable:(int)value {
    _instanceVariable = value;
}

- (int)instanceVariable {
    return _instanceVariable;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyClass *object = [[MyClass alloc] init];
        [object setInstanceVariable:42];

        int value = [object instanceVariable];
        NSLog(@"The value of the instance variable is %d", value);
    }

    return 0;
}
```

This code defines a simple Objective-C class called `MyClass` that has an instance variable named `_instanceVariable`. The class has two methods: `setInstanceVariable:` and `instanceVariable`, which allow the instance variable to be set and retrieved, respectively.

The `main` function creates an instance of `MyClass`, sets the instance variable to the value 42, and then retrieves the value of the instance variable and prints it to the console.

The code is written in a structured and easy-to-read manner, with each method and variable clearly defined and documented. The use of Objective-C's dot syntax for accessing instance variables and methods makes the code concise and readable.

Overall, this code is a good example of how to write clean and maintainable Objective-C code.