Certainly! Below is a complex and unique Objective-C code that demonstrates a custom implementation of a data structure called a Skip List. 

```objective-c
#import <Foundation/Foundation.h>

// Node class representing a single node in the skip list
@interface Node : NSObject

@property (nonatomic, strong) NSNumber *value;
@property (nonatomic, strong) Node *next;
@property (nonatomic, strong) Node *down;

- (instancetype)initWithValue:(NSNumber *)value;
- (instancetype)initWithValue:(NSNumber *)value next:(Node *)next down:(Node *)down;

@end

@implementation Node

- (instancetype)initWithValue:(NSNumber *)value {
    return [self initWithValue:value next:nil down:nil];
}

- (instancetype)initWithValue:(NSNumber *)value next:(Node *)next down:(Node *)down {
    self = [super init];
    if (self) {
        _value = value;
        _next = next;
        _down = down;
    }
    return self;
}

@end

// Skip List implementation
@interface SkipList : NSObject

@property (nonatomic, strong) Node *head;
@property (nonatomic) NSUInteger maxLevel;
@property (nonatomic) NSUInteger size;

- (instancetype)initWithMaxLevel:(NSUInteger)maxLevel;
- (void)insert:(NSNumber *)value;
- (BOOL)contains:(NSNumber *)value;
- (void)remove:(NSNumber *)value;
- (void)printSkipList;

@end

@implementation SkipList

- (instancetype)initWithMaxLevel:(NSUInteger)maxLevel {
    self = [super init];
    if (self) {
        _head = [[Node alloc] initWithValue:nil];
        _maxLevel = maxLevel;
        _size = 0;
    }
    return self;
}

- (void)insert:(NSNumber *)value {
    NSUInteger level = 0;
    while (level <= self.maxLevel && (arc4random_uniform(2) == 1)) {
        if (level == self.maxLevel) {
            self.maxLevel++;
            Node *newHead = [[Node alloc] initWithValue:nil next:nil down:self.head];
            self.head = newHead;
        }
        level++;
    }
    
    Node *current = self.head;
    Node *lastInserted = nil;
    
    while (current) {
        if (current.next == nil || [current.next.value compare:value] == NSOrderedDescending) {
            if (level >= current.level) {
                Node *newNode = [[Node alloc] initWithValue:value next:current.next down:lastInserted];
                current.next = newNode;
                lastInserted = newNode;
            }
            current = current.down;
        } else {
            current = current.next;
        }
    }
    
    self.size++;
}

- (BOOL)contains:(NSNumber *)value {
    Node *current = self.head;
    
    while (current) {
        if (current.next == nil || [current.next.value compare:value] == NSOrderedDescending) {
            current = current.down;
        } else if ([current.next.value compare:value] == NSOrderedSame) {
            return YES;
        } else {
            current = current.next;
        }
    }
    
    return NO;
}

- (void)remove:(NSNumber *)value {
    Node *current = self.head;
    BOOL removed = NO;
    
    while (current) {
        if (current.next == nil || [current.next.value compare:value] == NSOrderedDescending) {
            current = current.down;
        } else if ([current.next.value compare:value] == NSOrderedSame) {
            current.next = current.next.next;
            removed = YES;
            current = current.down;
        } else {
            current = current.next;
        }
    }
    
    if (removed) {
        self.size--;
    }
}

- (void)printSkipList {
    Node *current = self.head;
    
    while (current) {
        Node *node = current.next;
        NSMutableString *line = [NSMutableString new];
        
        while (node) {
            [line appendString:[NSString stringWithFormat:@"%@ -> ", node.value]];
            node = node.next;
        }
        
        NSLog(@"%@", line);
        
        current = current.down;
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        SkipList *skipList = [[SkipList alloc] initWithMaxLevel:4];
        
        [skipList insert:@10];
        [skipList insert:@20];
        [skipList insert:@15];
        [skipList insert:@5];
        [skipList insert:@30];
        
        [skipList printSkipList];
        
        NSLog(@"Contains 10: %d", [skipList contains:@10]);
        NSLog(@"Contains 25: %d", [skipList contains:@25]);
        
        [skipList remove:@15];
        [skipList remove:@20];
        
        [skipList printSkipList];
    }
    return 0;
}
```

This code implements a Skip List data structure in Objective-C. A Skip List is a probabilistic data structure that allows for efficient searching, insertion, and removal operations in logarithmic time complexity.

The `Node` class represents a single node in the Skip List, containing a value, references to the next node on the same level and the next node on the level below.

The `SkipList` class implements the Skip List data structure and provides methods for inserting, checking if a value is contained, removing, and printing the Skip List. The `insert:` method randomly decides the level at which the value should be inserted and creates new nodes accordingly. The `contains:` method traverses the Skip List to check if a value is present. The `remove:` method removes a value from the Skip List if it exists. The `printSkipList` method prints each level of the Skip List.

In the `main` function, a `SkipList` instance is created with a maximum level of 4. Several values are then inserted, and the Skip List is printed. The code also demonstrates checking if specific values are contained in the Skip List and removing values from it.

Please note that this is a simplified implementation of a Skip List and may not have all the optimizations found in production-ready code.