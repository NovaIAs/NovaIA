**Objective-C Code:**

```objective-c
#import <Foundation/Foundation.h>

@interface Calculator : NSObject

- (double)add:(double)operand1 with:(double)operand2;
- (double)subtract:(double)operand1 with:(double)operand2;
- (double)multiply:(double)operand1 with:(double)operand2;
- (double)divide:(double)operand1 with:(double)operand2;

@end

@implementation Calculator

- (double)add:(double)operand1 with:(double)operand2 {
    return operand1 + operand2;
}

- (double)subtract:(double)operand1 with:(double)operand2 {
    return operand1 - operand2;
}

- (double)multiply:(double)operand1 with:(double)operand2 {
    return operand1 * operand2;
}

- (double)divide:(double)operand1 with:(double)operand2 {
    if (operand2 == 0) {
        @throw [NSException exceptionWithName:@"Division by Zero" reason:@"Cannot divide by zero" userInfo:nil];
    }
    return operand1 / operand2;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Calculator *calculator = [[Calculator alloc] init];
        
        double result = [calculator add:10.5 with:5.2];
        NSLog(@"Addition Result: %f", result);
        
        result = [calculator subtract:15.3 with:7.6];
        NSLog(@"Subtraction Result: %f", result);
        
        result = [calculator multiply:4.8 with:2.7];
        NSLog(@"Multiplication Result: %f", result);
        
        result = [calculator divide:20.0 with:4.0];
        NSLog(@"Division Result: %f", result);
        
        // Uncomment the following line to see the division by zero exception
        // result = [calculator divide:10.0 with:0.0];
    }
    return 0;
}
```

**Explanation:**

1. **Objective-C Class Definition:**
   - We define a class called `Calculator` that encapsulates the functionality of a simple calculator.

2. **Interface Declaration:**
   - The `@interface` section declares the class name, its properties, and its methods. In this case, we declare four methods: `add`, `subtract`, `multiply`, and `divide`.

3. **Method Implementations:**
   - The `@implementation` section provides the actual implementations of the methods declared in the interface. Each method performs the corresponding arithmetic operation on the specified operands.

4. **Main Function:**
   - The `main` function is the entry point of the program.

5. **Calculator Object and Method Invocation:**
   - We create an instance of the `Calculator` class and invoke its methods to perform various arithmetic operations. The results are printed to the console using `NSLog`.

6. **Division by Zero Exception Handling:**
   - We handle the case of division by zero by throwing an exception. If division by zero is attempted, the program will terminate with an error message.

This code demonstrates the creation of a simple calculator class in Objective-C, along with error handling for division by zero. It represents a more complex and practical example of Objective-C code compared to the basic "Hello, World!" program.