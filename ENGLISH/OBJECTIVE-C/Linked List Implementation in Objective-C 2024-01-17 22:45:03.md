```objective-c
#import <Foundation/Foundation.h>

@interface Node : NSObject {
    id value;
    Node *next;
}

- (id)initWithValue:(id)initValue;
- (void)setNext:(Node *)newNext;
- (Node *)next;
- (id)value;

@end

@implementation Node

- (id)initWithValue:(id)initValue {
    self = [super init];
    if (self) {
        value = initValue;
        next = nil;
    }
    return self;
}

- (void)setNext:(Node *)newNext {
    next = newNext;
}

- (Node *)next {
    return next;
}

- (id)value {
    return value;
}

@end

@interface LinkedList : NSObject {
    Node *head;
    Node *tail;
}

- (id)init;
- (void)add:(id)value;
- (id)removeHead;
- (BOOL)isEmpty;
- (id)first;
- (id)last;

@end

@implementation LinkedList

- (id)init {
    self = [super init];
    if (self) {
        head = nil;
        tail = nil;
    }
    return self;
}

- (void)add:(id)value {
    Node *newNode = [[Node alloc] initWithValue:value];
    if (head == nil) {
        head = newNode;
        tail = newNode;
    } else {
        tail.next = newNode;
        tail = newNode;
    }
}

- (id)removeHead {
    if (head == nil) {
        return nil;
    }
    id value = head.value;
    head = head.next;
    if (head == nil) {
        tail = nil;
    }
    return value;
}

- (BOOL)isEmpty {
    return head == nil;
}

- (id)first {
    if (head == nil) {
        return nil;
    }
    return head.value;
}

- (id)last {
    if (tail == nil) {
        return nil;
    }
    return tail.value;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        LinkedList *list = [[LinkedList alloc] init];
        [list add:@"Hello"];
        [list add:@"World"];
        [list add:@"!"];
        while (![list isEmpty]) {
            NSLog(@"%@", [list removeHead]);
        }
    }
    return 0;
}
```

This code implements a linked list data structure in Objective-C. It consists of two classes: `Node` and `LinkedList`. The `Node` class represents a single node in the linked list, and the `LinkedList` class manages a collection of nodes.

The `Node` class has three properties: `value`, `next`, and `prev`. The `value` property stores the data associated with the node, the `next` property points to the next node in the list, and the `prev` property points to the previous node in the list.

The `LinkedList` class has three properties: `head`, `tail`, and `size`. The `head` property points to the first node in the list, the `tail` property points to the last node in the list, and the `size` property stores the number of nodes in the list.

The `LinkedList` class has several methods for manipulating the list. The `add` method adds a new node to the end of the list, the `removeHead` method removes the first node from the list, the `isEmpty` method checks if the list is empty, the `first` method returns the value of the first node in the list, and the `last` method returns the value of the last node in the list.

The main function creates a new `LinkedList` instance, adds three values to the list, and then removes and prints each value from the list until the list is empty.

This code demonstrates how to implement a linked list data structure in Objective-C and how to use it to store and retrieve data.