```objective-c
#import <Foundation/Foundation.h>

// Define a complex data structure to hold various types of information
@interface ComplexDataStructure : NSObject
{
    // Declare member variables
    NSString *_name;
    NSInteger _age;
    NSArray *_hobbies;
    NSDictionary *_address;
    NSMutableArray *_friends;
}

// Declare properties for accessing member variables
@property (nonatomic, copy) NSString *name;
@property (nonatomic) NSInteger age;
@property (nonatomic, copy) NSArray *hobbies;
@property (nonatomic, copy) NSDictionary *address;
@property (nonatomic, strong) NSMutableArray *friends;

// Declare initializer method
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age hobbies:(NSArray *)hobbies address:(NSDictionary *)address friends:(NSMutableArray *)friends;

// Declare other methods for accessing and modifying data
- (void)addFriend:(NSString *)friendName;
- (void)removeFriend:(NSString *)friendName;
- (void)printAllFriends;
@end

// Implement the ComplexDataStructure class
@implementation ComplexDataStructure

// Implement initializer method
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age hobbies:(NSArray *)hobbies address:(NSDictionary *)address friends:(NSMutableArray *)friends
{
    self = [super init];
    if (self) {
        _name = name;
        _age = age;
        _hobbies = hobbies;
        _address = address;
        _friends = friends;
    }
    return self;
}

// Implement method to add a friend to the friends array
- (void)addFriend:(NSString *)friendName
{
    [_friends addObject:friendName];
}

// Implement method to remove a friend from the friends array
- (void)removeFriend:(NSString *)friendName
{
    [_friends removeObject:friendName];
}

// Implement method to print all friends in the friends array
- (void)printAllFriends
{
    NSLog(@"Friends:");
    for (NSString *friendName in _friends) {
        NSLog(@"%@", friendName);
    }
}
@end

// Define a protocol for defining custom sorting behavior
@protocol CustomSortingProtocol

// Declare method for comparing two objects
- (NSComparisonResult)compare:(id)object;

@end

// Define a class that conforms to the CustomSortingProtocol
@interface CustomSortingClass : NSObject <CustomSortingProtocol>

// Declare member variables
NSString *_value;

// Declare initializer method
- (instancetype)initWithValue:(NSString *)value;

// Implement method for comparing two objects
- (NSComparisonResult)compare:(id)object;

@end

// Implement the CustomSortingClass class
@implementation CustomSortingClass

// Implement initializer method
- (instancetype)initWithValue:(NSString *)value
{
    self = [super init];
    if (self) {
        _value = value;
    }
    return self;
}

// Implement method for comparing two objects
- (NSComparisonResult)compare:(id)object
{
    CustomSortingClass *otherObject = (CustomSortingClass *)object;
    return [_value compare:otherObject->_value];
}
@end

// Define a class for demonstrating various Objective-C features
@interface ComplexClass : NSObject

// Declare member variables
NSString *_name;
NSInteger _age;
NSArray *_hobbies;
NSDictionary *_address;
NSMutableArray *_friends;
NSSet *_colleagues;
CustomSortingClass *_sortingObject;

// Declare properties for accessing member variables
@property (nonatomic, copy) NSString *name;
@property (nonatomic) NSInteger age;
@property (nonatomic, copy) NSArray *hobbies;
@property (nonatomic, copy) NSDictionary *address;
@property (nonatomic, strong) NSMutableArray *friends;
@property (nonatomic, copy) NSSet *colleagues;
@property (nonatomic, strong) CustomSortingClass *sortingObject;

// Declare initializer method
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age hobbies:(NSArray *)hobbies address:(NSDictionary *)address friends:(NSMutableArray *)friends colleagues:(NSSet *)colleagues sortingObject:(CustomSortingClass *)sortingObject;

// Declare other methods for accessing and modifying data
- (void)addFriend:(NSString *)friendName;
- (void)removeFriend:(NSString *)friendName;
- (void)printAllFriends;
- (void)sortCustomObjects;
@end

// Implement the ComplexClass class
@implementation ComplexClass

// Implement initializer method
- (instancetype)initWithName:(NSString *)name age:(NSInteger)age hobbies:(NSArray *)hobbies address:(NSDictionary *)address friends:(NSMutableArray *)friends colleagues:(NSSet *)colleagues sortingObject:(CustomSortingClass *)sortingObject
{
    self = [super init];
    if (self) {
        _name = name;
        _age = age;
        _hobbies = hobbies;
        _address = address;
        _friends = friends;
        _colleagues = colleagues;
        _sortingObject = sortingObject;
    }
    return self;
}

// Implement method to add a friend to the friends array
- (void)addFriend:(NSString *)friendName
{
    [_friends addObject:friendName];
}

// Implement method to remove a friend from the friends array
- (void)removeFriend:(NSString *)friendName
{
    [_friends removeObject:friendName];
}

// Implement method to print all friends in the friends array
- (void)printAllFriends
{
    NSLog(@"Friends:");
    for (NSString *friendName in _friends) {
        NSLog(@"%@", friendName);
    }
}

// Implement method to sort custom objects using the sortingObject
- (void)sortCustomObjects
{
    // Create an array of custom objects
    NSArray *customObjects = @[[[CustomSortingClass alloc] initWithValue:@"Object 1"],
                               [[CustomSortingClass alloc] initWithValue:@"Object 3"],
                               [[CustomSortingClass alloc] initWithValue:@"Object 2"]];
    
    // Sort the array using the sortingObject
    NSArray *sortedObjects = [customObjects sortedArrayUsingSelector:@selector(compare:)];
    
    // Print the sorted array
    NSLog(@"Sorted Custom Objects:");
    for (CustomSortingClass *object in sortedObjects) {
        NSLog(@"%@", object->_value);
    }
}
@end

// Define a category for extending the NSString class with a custom method
@interface NSString (MyCategory)

// Declare custom method
- (NSString *)reverseString;

@end

// Implement the category for NSString
@implementation NSString (MyCategory)

// Implement custom method for reversing a string
- (NSString *)reverseString
{
    NSMutableString *reversedString = [NSMutableString stringWithCapacity:[self length]];
    for (NSInteger i = [self length] - 1; i >= 0; i--) {
        [reversedString appendFormat:@"%C", [self characterAtIndex:i]];
    }
    return reversedString;
}

@end

// Define a block for performing a task
void (^myBlock)(NSString *) = ^(NSString *input) {
    NSLog(@"Reversed String: %@", [input reverseString]);
};

// Define a GCD queue for performing asynchronous tasks
dispatch_queue_t myQueue = dispatch_queue_create("com.example.myQueue", DISPATCH_QUEUE_CONCURRENT);

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an instance of ComplexDataStructure
        ComplexDataStructure *dataStructure = [[ComplexDataStructure alloc] initWithName:@"John" age:30 hobbies:@[@"Reading", @"Hiking", @"Coding"] address:@{@"Street": @"123 Main Street", @"City": @"Anytown", @"State": @"CA", @"Zip": @"12345"} friends:[NSMutableArray arrayWithArray:@[@"Mary", @"Bob", @"Alice"]]];
        
        // Add a friend to the friends array
        [dataStructure addFriend:@"Tom"];
        
        // Remove a friend from the friends array
        [dataStructure removeFriend:@"Bob"];
        
        // Print all friends in the friends array
        [dataStructure printAllFriends];
        
        // Create an instance of ComplexClass
        ComplexClass *complexObject = [[ComplexClass alloc] initWithName:@"Jane" age:25 hobbies:@[@"Dancing", @"Singing", @"Traveling"] address:@{@"Street": @"456 Elm Street", @"City": @"Somewhere", @"State": @"NY", @"Zip": @"54321"} friends:[NSMutableArray arrayWithArray:@[@"Jack", @"Jill", @"Peter"]