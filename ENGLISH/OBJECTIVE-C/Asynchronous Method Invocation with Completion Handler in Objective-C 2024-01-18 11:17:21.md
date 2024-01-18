```objective-c
@interface MyClass : NSObject {
    // Private data
}

- (instancetype)initWithParameters:(NSDictionary *)parameters;

- (void)doSomethingWithCompletionHandler:(void (^)(NSError *error))completionHandler;

@end

@implementation MyClass

- (instancetype)initWithParameters:(NSDictionary *)parameters {
    if (self = [super init]) {
        // Initialize private data
    }
    return self;
}

- (void)doSomethingWithCompletionHandler:(void (^)(NSError *error))completionHandler {
    // Do some asynchronous operation
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Do some work
        if (success) {
            completionHandler(nil);
        } else {
            completionHandler([NSError errorWithDomain:@"MyDomain" code:-1 userInfo:nil]);
        }
    });
}

@end

// Usage

MyClass *myClass = [[MyClass alloc] initWithParameters:@{}];
[myClass doSomethingWithCompletionHandler:^(NSError *error) {
    if (error) {
        // Handle error
    } else {
        // Operation completed successfully
    }
}];
```

Explanation:

1. `@interface MyClass : NSObject`: This line declares the MyClass class, which inherits from NSObject.

2. `- (instancetype)initWithParameters:(NSDictionary *)parameters;`: This line declares the initializer method for MyClass, which takes a dictionary of parameters.

3. `- (void)doSomethingWithCompletionHandler:(void (^)(NSError *error))completionHandler;`: This line declares a method named `doSomethingWithCompletionHandler` which takes a completion handler block as an argument. The completion handler block is called when the asynchronous operation is complete.

4. In the `initWithParameters` method, the `self = [super init]` line calls the initializer method of the NSObject superclass. The if statement ensures that the `self` object is not nil before initializing the private data.

5. In the `doSomethingWithCompletionHandler` method, the `dispatch_async` function is used to perform the asynchronous operation in a background thread.

6. Inside the `dispatch_async` block, the `if` statement checks if the operation was successful. If it was, the completion handler block is called with nil as the argument. If the operation failed, the completion handler block is called with an error object as the argument.

7. In the usage section, an instance of MyClass is created and the `doSomethingWithCompletionHandler` method is called with a completion handler block. The completion handler block handles the response from the asynchronous operation.