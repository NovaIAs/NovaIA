```objective-c
#import <Foundation/Foundation.h>

// Define a custom exception class
@interface MyException : NSException
@end

@implementation MyException
@end

// Define a protocol for a stack data structure
@protocol Stack
- (void)push:(id)object;
- (id)pop;
- (id)peek;
@end

// Implement a stack using an array
@interface ArrayStack : NSObject <Stack>
{
    NSMutableArray *_array;
}

- (ArrayStack *)init;
- (void)push:(id)object;
- (id)pop;
- (id)peek;
@end

@implementation ArrayStack

- (ArrayStack *)init
{
    self = [super init];
    if (self) {
        _array = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)push:(id)object
{
    [_array addObject:object];
}

- (id)pop
{
    if ([_array count] == 0) {
        // Throw an exception if the stack is empty
        @throw [[MyException alloc] init];
    }
    id object = [_array lastObject];
    [_array removeLastObject];
    return object;
}

- (id)peek
{
    if ([_array count] == 0) {
        // Throw an exception if the stack is empty
        @throw [[MyException alloc] init];
    }
    return [_array lastObject];
}

@end

// Define a tree node data structure
@interface TreeNode
{
    id _value;
    TreeNode *_left;
    TreeNode *_right;
}

- (TreeNode *)initWithValue:(id)value;
- (void)setLeft:(TreeNode *)left;
- (void)setRight:(TreeNode *)right;
- (id)value;
- (TreeNode *)left;
- (TreeNode *)right;
@end

@implementation TreeNode

- (TreeNode *)initWithValue:(id)value
{
    self = [super init];
    if (self) {
        _value = value;
        _left = nil;
        _right = nil;
    }
    return self;
}

- (void)setLeft:(TreeNode *)left
{
    _left = left;
}

- (void)setRight:(TreeNode *)right
{
    _right = right;
}

- (id)value
{
    return _value;
}

- (TreeNode *)left
{
    return _left;
}

- (TreeNode *)right
{
    return _right;
}

@end

// Implement a binary search tree using the tree node data structure
@interface BinarySearchTree
{
    TreeNode *_root;
}

- (BinarySearchTree *)init;
- (void)insert:(id)value;
- (TreeNode *)search:(id)value;
- (void)remove:(id)value;
@end

@implementation BinarySearchTree

- (BinarySearchTree *)init
{
    self = [super init];
    if (self) {
        _root = nil;
    }
    return self;
}

- (void)insert:(id)value
{
    TreeNode *new