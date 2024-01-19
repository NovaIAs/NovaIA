```objective-c
// Welcome! This is a complex and differentiated code that demonstrates various aspects of Objective-C.

// We start with importing necessary frameworks and libraries.
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Let's define a custom class called "ComplexObject".
@interface ComplexObject : NSObject

// Declare private instance variables.
@private
    int _intValue;
    NSString *_stringValue;
    NSArray *_arrayValue;
    NSDictionary *_dictionaryValue;

// Declare public properties for accessing instance variables.
@property int intValue;
@property NSString *stringValue;
@property NSArray *arrayValue;
@property NSDictionary *dictionaryValue;

// Define an initializer method.
- (instancetype)initWithIntValue:(int)intValue
                      stringValue:(NSString *)stringValue
                       arrayValue:(NSArray *)arrayValue
                    dictionaryValue:(NSDictionary *)dictionaryValue;

// Declare instance methods.
- (void)printValues;

@end

// Implementation of the ComplexObject class.
@implementation ComplexObject

// Define the initializer method implementation.
- (instancetype)initWithIntValue:(int)intValue
                      stringValue:(NSString *)stringValue
                       arrayValue:(NSArray *)arrayValue
                    dictionaryValue:(NSDictionary *)dictionaryValue {
    self = [super init]; // Call the superclass's initializer.
    if (self) {
        _intValue = intValue;
        _stringValue = stringValue;
        _arrayValue = arrayValue;
        _dictionaryValue = dictionaryValue;
    }
    return self;
}

// Define the implementation of the printValues method.
- (void)printValues {
    NSLog(@"Int Value: %d", _intValue);
    NSLog(@"String Value: %@", _stringValue);
    NSLog(@"Array Value: %@", _arrayValue);
    NSLog(@"Dictionary Value: %@", _dictionaryValue);
}

@end

// Now, let's create an instance of the ComplexObject class and use it.
ComplexObject *complexObject = [[ComplexObject alloc] initWithIntValue:10
                                                           stringValue:@"Hello, World!"
                                                            arrayValue:@[@1, @2, @3]
                                                         dictionaryValue:@{@"key1": @"value1", @"key2": @"value2"}];

// Print the values of the complex object.
[complexObject printValues];

// Let's also demonstrate some Objective-C features like blocks and GCD.

// Define a block that takes an integer and returns its square.
int (^squareBlock)(int) = ^(int number) {
    return number * number;
};

// Use GCD to perform a task asynchronously in the background.
dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
    // Perform some calculations or tasks in the background.
    int result = squareBlock(5);

    // After the task is completed, update the UI on the main thread.
    dispatch_async(dispatch_get_main_queue(), ^{
        // Update a UI element or perform other UI-related tasks.
        NSLog(@"Result: %d", result);
    });
});

// This code demonstrates various concepts in Objective-C, including custom classes, properties, methods, blocks, and GCD. It's a complex and differentiated code that showcases the power and flexibility of Objective-C.
```