```objective-c
#import <UIKit/UIKit.h>

@interface MyComplexClass : UIViewController

// Declare private properties
@property (nonatomic, strong) NSArray *myArray;
@property (nonatomic, strong) NSDictionary *myDictionary;
@property (nonatomic, strong) NSString *myString;

// Declare public methods
- (void)doSomething;

@end

@implementation MyComplexClass

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Initialize private properties
    self.myArray = @[@"item1", @"item2", @"item3"];
    self.myDictionary = @{@"key1": @"value1", @"key2": @"value2", @"key3": @"value3"};
    self.myString = @"Hello, world!";
    
    // Call public method
    [self doSomething];
}

- (void)doSomething {
    // Use private properties
    for (NSString *item in self.myArray) {
        NSLog(@"Item: %@", item);
    }
    
    for (NSString *key in self.myDictionary.allKeys) {
        NSLog(@"Key: %@, Value: %@", key, self.myDictionary[key]);
    }
    
    NSLog(@"String: %@", self.myString);
}

@end
```

Explanation:

1. **Import the UIKit Framework**:
   ```objective-c
   #import <UIKit/UIKit.h>
   ```
   This line imports the UIKit framework, which provides the essential classes and protocols for developing iOS applications.

2. **Declare the `MyComplexClass` Interface**:
   ```objective-c
   @interface MyComplexClass : UIViewController
   ```
   This line declares the interface for the `MyComplexClass` class, which is a subclass of `UIViewController`. This means that `MyComplexClass` can be used as a view controller in an iOS application.

3. **Declare Private Properties**:
   ```objective-c
   @property (nonatomic, strong) NSArray *myArray;
   @property (nonatomic, strong) NSDictionary *myDictionary;
   @property (nonatomic, strong) NSString *myString;
   ```
   These lines declare three private properties for the `MyComplexClass` class:
   - `myArray`: An array of strings.
   - `myDictionary`: A dictionary of strings.
   - `myString`: A string.

4. **Declare Public Methods**:
   ```objective-c
   - (void)doSomething;
   ```
   This line declares a public method named `doSomething` for the `MyComplexClass` class. This method can be called from other classes to perform a task.

5. **Implement the `viewDidLoad` Method**:
   ```objective-c
   - (void)viewDidLoad {
       [super viewDidLoad];
   
       // Initialize private properties
       self.myArray = @[@"item1", @"item2", @"item3"];
       self.myDictionary = @{@"key1": @"value1", @"key2": @"value2", @"key3": @"value3"};
       self.myString = @"Hello, world!";
   
       // Call public method
       [self doSomething];
   }
   ```
   This method is called when the view controller's view is loaded into memory. Here, we initialize the private properties with some data and then call the `doSomething` method.

6. **Implement the `doSomething` Method**:
   ```objective-c
   - (void)doSomething {
       // Use private properties
       for (NSString *item in self.myArray) {
           NSLog(@"Item: %@", item);
       }
   
       for (NSString *key in self.myDictionary.allKeys) {
           NSLog(@"Key: %@, Value: %@", key, self.myDictionary[key]);
       }
   
       NSLog(@"String: %@", self.myString);
   }
   ```
   This method uses the private properties to perform some tasks:
   - It iterates through the `myArray` array and prints each item to the console.
   - It iterates through the `myDictionary` dictionary and prints each key-value pair to the console.
   - It prints the `myString` string to the console.

7. **End of Class Implementation**:
   ```objective-c
   @end
   ```
   This line marks the end of the implementation for the `MyComplexClass` class.