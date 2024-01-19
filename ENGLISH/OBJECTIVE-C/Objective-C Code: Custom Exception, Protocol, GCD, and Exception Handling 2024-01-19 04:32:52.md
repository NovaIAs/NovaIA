Objective-C Code:

```objective-c
#import <Foundation/Foundation.h>

// Define a custom exception class
@interface CustomException : NSException
@end

@implementation CustomException
@end

// Define a protocol to enforce certain methods in classes
@protocol MyProtocol
- (void)someMethod;
@end

// Define a class that conforms to the protocol
@interface MyClass : NSObject <MyProtocol>
- (void)someMethod;
@end

@implementation MyClass
- (void)someMethod {
    // Complex implementation
}
@end

// Define a block type for use with GCD
typedef void (^MyBlock)(void);

// Define a function to be executed on a background queue
void backgroundTask(void) {
    // Complex background task
}

// Main function
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an instance of MyClass
        MyClass *myObject = [[MyClass alloc] init];
        
        // Call the method on the object
        [myObject someMethod];
        
        // Create a GCD queue and dispatch the background task
        dispatch_queue_t myQueue = dispatch_queue_create("com.example.myQueue", DISPATCH_QUEUE_CONCURRENT);
        dispatch_async(myQueue, ^{
            backgroundTask();
        });
        
        // Handle potential exceptions
        @try {
            // Code that may throw an exception
        } @catch (CustomException *exception) {
            // Exception handling code
        } @finally {
            // Cleanup code
        }
    }
    return 0;
}
```

Explanation:

1. **Custom Exception:**
   - We define a custom exception class called `CustomException` that inherits from `NSException`.
   - This allows us to create our own custom exceptions and handle them specifically in our code.

2. **Protocol and Class:**
   - We define a protocol `MyProtocol` with a method `someMethod`.
   - We create a class `MyClass` that conforms to the protocol and implements the `someMethod` method.

3. **GCD and Block:**
   - We define a block type `MyBlock` that takes no arguments and returns nothing.
   - We define a function `backgroundTask` that performs a complex background task and is executed on a GCD queue.

4. **Main Function:**
   - We create an instance of `MyClass` and call the `someMethod` method on it.
   - We create a GCD queue and dispatch the `backgroundTask` function to run on the queue concurrently.
   - We use exception handling to catch and handle potential exceptions thrown by the code.

This code demonstrates complex and diverse functionality in Objective-C, including custom exceptions, protocols, GCD, and exception handling.