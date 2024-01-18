```objective-c
#import <UIKit/UIKit.h>

@interface ComplexClass : NSObject

// Properties
@property (nonatomic, strong) NSArray *items;
@property (nonatomic, assign) NSInteger currentIndex;

// Methods
- (instancetype)initWithItems:(NSArray *)items;
- (void)nextItem;
- (void)previousItem;
- (id)currentItem;

@end

@implementation ComplexClass

- (instancetype)initWithItems:(NSArray *)items {
    self = [super init];
    if (self) {
        self.items = items;
    }
    return self;
}

- (void)nextItem {
    self.currentIndex++;
    if (self.currentIndex >= self.items.count) {
        self.currentIndex = 0;
    }
}

- (void)previousItem {
    self.currentIndex--;
    if (self.currentIndex < 0) {
        self.currentIndex = self.items.count - 1;
    }
}

- (id)currentItem {
    return self.items[self.currentIndex];
}

@end

// Usage

NSArray *items = @[@"Item 1", @"Item 2", @"Item 3"];
ComplexClass *complexClass = [[ComplexClass alloc] initWithItems:items];

NSLog(@"%@", complexClass.currentItem); // Output: "Item 1"

[complexClass nextItem];

NSLog(@"%@", complexClass.currentItem); // Output: "Item 2"

[complexClass previousItem];

NSLog(@"%@", complexClass.currentItem); // Output: "Item 1"
```

Explanation:

1. The ComplexClass interface declares the properties and methods of the class.
2. The ComplexClass implementation defines the methods of the class.
3. The initWithItems: method initializes the class with an array of items.
4. The nextItem and previousItem methods move the current index to the next or previous item in the array, respectively.
5. The currentItem method returns the current item in the array.
6. The usage section shows how to use the ComplexClass.
7. The NSLog statements print the current item to the console.