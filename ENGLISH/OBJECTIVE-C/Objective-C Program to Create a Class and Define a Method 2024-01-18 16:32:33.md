```objective-c
#import <Foundation/Foundation.h>

@interface MyClass : NSObject

- (void)doSomething;

@end

@implementation MyClass

- (void)doSomething {
    // Do something here
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyClass *myClass = [[MyClass alloc] init];
        [myClass doSomething];
    }
    return 0;
}
```

This is a simple Objective-C program that creates a class called `MyClass` and defines a method called `doSomething`. The `main` function creates an instance of `MyClass` and calls the `doSomething` method.

Here's a breakdown of the code:

1. Import the Foundation framework:

```
#import <Foundation/Foundation.h>
```
This line imports the Foundation framework, which provides the basic functionality for Objective-C programs.

2. Declare the `MyClass` class:

```
@interface MyClass : NSObject
```
This line declares a new class called `MyClass`. It inherits from the `NSObject` class, which is the base class for all Objective-C objects.

3. Define the `doSomething` method:

```
- (void)doSomething;
```
This line defines a method called `doSomething` in the `MyClass` class. The method does not take any arguments and returns nothing.

4. Implement the `doSomething` method:

```
@implementation MyClass

- (void)doSomething {
    // Do something here
}

@end
```
This code implements the `doSomething` method. It simply prints a message to the console.

5. Create an instance of `MyClass` and call the `doSomething` method:

```
MyClass *myClass = [[MyClass alloc] init];
[myClass doSomething];
```
This code creates an instance of the `MyClass` class and calls the `doSomething` method on that instance.

6. Return 0 from the `main` function:

```
return 0;
```
This line returns 0 from the `main` function to indicate that the program ran successfully.

This code is fairly simple, but it demonstrates the basic structure of an Objective-C program. You can use this code as a starting point for creating more complex Objective-C programs.