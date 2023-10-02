Claro! Aqui está um exemplo de código em Objective-C que cria uma árvore binária de busca balanceada:

```
#import <Foundation/Foundation.h>

@interface Node : NSObject

@property (nonatomic, strong) id data;
@property (nonatomic, strong) Node *left;
@property (nonatomic, strong) Node *right;

@end

@implementation Node

@end

@interface BinarySearchTree : NSObject

@property (nonatomic, strong) Node *root;

- (void)insert:(id)data;
- (BOOL)search:(id)data;
- (void)printTree;

@end

@implementation BinarySearchTree

- (void)insert:(id)data {
    Node *newNode = [[Node alloc] init];
    newNode.data = data;
    
    if (self.root == nil) {
        self.root = newNode;
    } else {
        [self insertNode:self.root newNode:newNode];
    }
}

- (void)insertNode:(Node *)node newNode:(Node *)newNode {
    if ([newNode.data intValue] < [node.data intValue]) {
        if (node.left == nil) {
            node.left = newNode;
        } else {
            [self insertNode:node.left newNode:newNode];
        }
    } else {
        if (node.right == nil) {
            node.right = newNode;
        } else {
            [self insertNode:node.right newNode:newNode];
        }
    }
}

- (BOOL)search:(id)data {
    return [self searchNode:self.root data:data];
}

- (BOOL)searchNode:(Node *)node data:(id)data {
    if (node == nil) {
        return NO;
    }
    
    if ([data intValue] == [node.data intValue]) {
        return YES;
    } else if ([data intValue] < [node.data intValue]) {
        return [self searchNode:node.left data:data];
    } else {
        return [self searchNode:node.right data:data];
    }
}

- (void)printTree {
    [self printNode:self.root];
}

- (void)printNode:(Node *)node {
    if (node != nil) {
        [self printNode:node.left];
        NSLog(@"%@", node.data);
        [self printNode:node.right];
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        BinarySearchTree *tree = [[BinarySearchTree alloc] init];
        
        [tree insert:@5];
        [tree insert:@3];
        [tree insert:@7];
        [tree insert:@2];
        [tree insert:@4];
        [tree insert:@6];
        [tree insert:@8];
        
        NSLog(@"Tree:");
        [tree printTree];
        
        NSLog(@"Search for 6: %d", [tree search:@6]);
        NSLog(@"Search for 9: %d", [tree search:@9]);
    }
    return 0;
}
```

Este código cria uma classe `Node` que representa um nó na árvore binária de busca e uma classe `BinarySearchTree` que contém as operações de inserção, busca e impressão da árvore.

No exemplo de `main`, criamos uma árvore binária de busca balanceada, inserimos alguns valores nela e em seguida realizamos uma busca pelos valores 6 e 9.

Espero que este código atenda às suas expectativas!