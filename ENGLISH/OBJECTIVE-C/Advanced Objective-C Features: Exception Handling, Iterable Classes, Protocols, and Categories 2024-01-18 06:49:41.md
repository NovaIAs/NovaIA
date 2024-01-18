```objective-c
#import <Foundation/Foundation.h>

// Custom Exception Class
@interface MyException : NSException
@end

@implementation MyException
@end

// Custom Iterable Class
@interface MySequence : NSObject <NSFastEnumeration>
@property(nonatomic, strong) NSArray *elements;

- (instancetype)initWithElements:(NSArray *)elements;
- (NSUInteger)countByEnumeratingWithState:(NSFastEnumerationState *)state
                                   objects:(id __unsafe_unretained *)buffer
                                     count:(NSUInteger)len;
@end

@implementation MySequence
- (instancetype)initWithElements:(NSArray *)elements {
    self = [super init];
    if (self) {
        _elements = elements;
    }
    return self;
}

- (NSUInteger)countByEnumeratingWithState:(NSFastEnumerationState *)state
                                   objects:(id __unsafe_unretained *)buffer
                                     count:(NSUInteger)len {
    NSUInteger count = self.elements.count;
    if (state->state == 0) {
        state->mutationsPtr = &state->extra[0];
    }
    state->itemsPtr = buffer;
    NSUInteger remaining = count - state->state;
    NSUInteger batchSize = MIN(len, remaining);
    for (NSUInteger i = 0; i < batchSize; i++) {
        buffer[i] = self.elements[state->state + i];
    }
    state->state += batchSize;
    state->extra[0] = 0;
    return batchSize;
}
@end

// Custom Protocol
@protocol MyProtocol <NSObject>
- (NSString *)formatString:(NSString *)input;
@end

@interface MyClass : NSObject <MyProtocol>
@end

@implementation MyClass
- (NSString *)formatString:(NSString *)input {
    return [input uppercaseString];
}
@end

// Custom Category
@interface NSArray (MyCategory)
- (NSArray *)reversedArray;
@end

@implementation NSArray (MyCategory)
- (NSArray *)reversedArray {
    NSMutableArray *reversedArray = [NSMutableArray arrayWithCapacity:self.count];
    NSEnumerator *enumerator = [self reverseObjectEnumerator];
    for (id element in enumerator) {
        [reversedArray addObject:element];
    }
    return reversedArray;
}
@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        // Exception Handling
        @try {
            // Raise an exception
            @throw [[MyException alloc] init];
        } @catch (MyException *exception) {
            // Handle the exception
            NSLog(@"Caught MyException: %@", exception);
        } @finally {
            // Always executed, even if an exception is thrown
            NSLog(@"Finally block executed");
        }

        // Iterable Class
        MySequence *sequence = [[MySequence alloc] initWithElements:@[@"one", @"two", @"three"]];
        for (NSString *element in sequence) {
            NSLog(@"Element: %@", element);
        }

        // Custom Protocol
        id<MyProtocol> myObject = [[MyClass alloc] init];
        NSString *formattedString = [myObject formatString:@"hello world"];
        NSLog(@"Formatted String: %@", formattedString);

        // Custom Category
        NSArray *array = @[@"a", @"b", @"c"];
        NSLog(@"Reversed Array: %@", [array reversedArray]);
    }
    return 0;
}
```

Explanation:

1. **Exception Handling**:
   - Exception Handling is implemented with `@try`, `@catch`, and `@finally` blocks.
   - Code that might throw an exception is placed in the `@try` block.
   - In the `@catch` block, we specify the type of exception to catch and handle it.
   - The `@finally` block is executed regardless of whether an exception is thrown or not.

2. **Iterable Class**:
   - This class, `MySequence`, implements the `NSFastEnumeration` protocol, making it iterable.
   - It takes an array of elements during initialization.
   - When iterated over, it returns its elements in order.

3. **Custom Protocol**:
   - `MyProtocol` defines a single method, `formatString`, which takes a string and returns a formatted string.
   - `MyClass` implements this protocol and provides its own implementation of `formatString`.

4. **Custom Category**:
   - `NSArray` is extended with a new method, `reversedArray`, using a category.
   - `reversedArray` returns a reversed copy of the original array.

The code demonstrates various advanced features of Objective-C, including custom classes, protocols, categories, and exception handling. It showcases how to create reusable components and extend the capabilities of built-in classes in Objective-C.