```objective-c
#import <Foundation/Foundation.h>

@interface MyObject : NSObject
{
    // Instance variables
}

- (void)myMethod;

@end

@implementation MyObject

- (void)myMethod
{
    // Method implementation
}

@end

int main(int argc, const char * argv[])
{
    @autoreleasepool
    {
        MyObject *myObject = [[MyObject alloc] init];
        [myObject myMethod];
    }
    
    return 0;
}
```

**Explanation:**

* The first line includes the Foundation framework, which contains basic classes for working with Objective-C.


* `MyObject` class is defined with an instance variable and a method called `myMethod`.


* In the `main` function, an instance of `MyObject` is created and the `myMethod` method is called.


* The `@autoreleasepool` block is used to automatically release any objects that are created within the block when the block is exited. This helps to prevent memory leaks.