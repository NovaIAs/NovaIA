```objective-c
#import <Foundation/Foundation.h>

// Define the main class
@interface ComplexCode : NSObject

// Define a method that takes a string and returns a reversed string
- (NSString *)reverseString:(NSString *)string;

// Define a method that takes an array of numbers and returns the sum of the numbers
- (int)sumOfArray:(NSArray *)array;

// Define a method that takes a dictionary and returns the value for a given key
- (id)getValueForKey:(NSDictionary *)dictionary key:(NSString *)key;

// Define a method that takes a block of code and executes it
- (void)executeBlock:(void (^)(void))block;

@end

// Implement the main class
@implementation ComplexCode

// Implement the method to reverse a string
- (NSString *)reverseString:(NSString *)string {
    NSMutableString *reversedString = [NSMutableString string];
    for (int i = string.length - 1; i >= 0; i--) {
        [reversedString appendString:[NSString stringWithFormat:@"%c", [string characterAtIndex:i]]];
    }
    return reversedString;
}

// Implement the method to sum an array of numbers
- (int)sumOfArray:(NSArray *)array {
    int sum = 0;
    for (NSNumber *number in array) {
        sum += [number intValue];
    }
    return sum;
}

// Implement the method to get a value from a dictionary
- (id)getValueForKey:(NSDictionary *)dictionary key:(NSString *)key {
    return [dictionary objectForKey:key];
}

// Implement the method to execute a block of code
- (void)executeBlock:(void (^)(void))block {
    block();
}

@end

// Create an instance of the main class
ComplexCode *code = [[ComplexCode alloc] init];

// Call the method to reverse a string
NSString *reversedString = [code reverseString:@"Hello, world!"];
NSLog(@"Reversed string: %@", reversedString);

// Call the method to sum an array of numbers
NSArray *numbers = @[@1, @2, @3, @4, @5];
int sum = [code sumOfArray:numbers];
NSLog(@"Sum of array: %d", sum);

// Call the method to get a value from a dictionary
NSDictionary *dictionary = @{@"name": @"John Doe", @"age": @30};
NSString *name = [code getValueForKey:dictionary key:@"name"];
NSLog(@"Name: %@", name);

// Call the method to execute a block of code
[code executeBlock:^{
    NSLog(@"This is a block of code that is being executed.");
}];
```

**Explanation:**

* The code defines a class called `ComplexCode` that has four methods:
    * `reverseString:` reverses a given string.
    * `sumOfArray:` calculates the sum of the numbers in a given array.
    * `getValueForKey:` gets the value for a given key in a dictionary.
    * `executeBlock:` executes a given block of code.
* The code creates an instance of the `ComplexCode` class and calls each of the methods to demonstrate their functionality.
* The code uses NSLog to print the results of the methods to the console.

**Notes:**

* The code uses Objective-C syntax and is intended to run on macOS or iOS platforms.
* The code uses Foundation framework, which provides basic functionality for working with objects and data structures.
* The code uses blocks, which are anonymous functions that can be passed as arguments to other functions or methods.
* The code uses the `@` prefix for Objective-C objects and the `#` prefix for Objective-C preprocessor directives.