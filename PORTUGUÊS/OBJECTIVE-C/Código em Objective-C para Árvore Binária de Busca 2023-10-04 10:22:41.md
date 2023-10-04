Claro! Aqui está um código complexo em Objective-C que cria uma árvore binária de busca e realiza algumas operações nela.

```objective-c
#import <Foundation/Foundation.h>

@interface Node : NSObject

@property (nonatomic, strong) NSNumber *value;
@property (nonatomic, strong) Node *leftChild;
@property (nonatomic, strong) Node *rightChild;

- (instancetype)initWithValue:(NSNumber *)value;
- (void)insertNode:(Node *)node;
- (BOOL)searchNodeWithValue:(NSNumber *)value;

@end

@implementation Node

- (instancetype)initWithValue:(NSNumber *)value {
    self = [super init];
    if (self) {
        _value = value;
        _leftChild = nil;
        _rightChild = nil;
    }
    return self;
}

- (void)insertNode:(Node *)node {
    if ([node.value isLessThan:self.value]) {
        if (self.leftChild) {
            [self.leftChild insertNode:node];
        } else {
            self.leftChild = node;
        }
    } else if ([node.value isGreaterThan:self.value]) {
        if (self.rightChild) {
            [self.rightChild insertNode:node];
        } else {
            self.rightChild = node;
        }
    }
}

- (BOOL)searchNodeWithValue:(NSNumber *)value {
    if ([value isEqualToNumber:self.value]) {
        return YES;
    } else if ([value isLessThan:self.value]) {
        if (self.leftChild) {
            return [self.leftChild searchNodeWithValue:value];
        }
    } else if ([value isGreaterThan:self.value]) {
        if (self.rightChild) {
            return [self.rightChild searchNodeWithValue:value];
        }
    }
    return NO;
}

@end

@interface BinarySearchTree : NSObject

@property (nonatomic, strong) Node *root;

- (instancetype)initWithRoot:(Node *)root;
- (void)insertValue:(NSNumber *)value;
- (BOOL)searchValue:(NSNumber *)value;

@end

@implementation BinarySearchTree

- (instancetype)initWithRoot:(Node *)root {
    self = [super init];
    if (self) {
        _root = root;
    }
    return self;
}

- (void)insertValue:(NSNumber *)value {
    Node *node = [[Node alloc] initWithValue:value];
    if (self.root) {
        [self.root insertNode:node];
    } else {
        self.root = node;
    }
}

- (BOOL)searchValue:(NSNumber *)value {
    if (self.root) {
        return [self.root searchNodeWithValue:value];
    }
    return NO;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        BinarySearchTree *tree = [[BinarySearchTree alloc] initWithRoot:nil];
        
        [tree insertValue:@5];
        [tree insertValue:@2];
        [tree insertValue:@7];
        [tree insertValue:@1];
        [tree insertValue:@4];
        [tree insertValue:@6];
        [tree insertValue:@8];
        
        BOOL found = [tree searchValue:@6];
        if (found) {
            NSLog(@"Valor encontrado na árvore.");
        } else {
            NSLog(@"Valor não encontrado na árvore.");
        }
    }
    return 0;
}
```

Neste código, criamos uma estrutura de dados de árvore binária de busca implementada em Objective-C. A classe `Node` representa um nó da árvore e contém um valor, bem como referências para os nós filhos esquerdo e direito. A classe `BinarySearchTree` representa a própria árvore e possui um nó raiz.

Dentro da classe `Node`, temos os métodos `insertNode:` que insere um novo nó na árvore de acordo com o valor, e `searchNodeWithValue:` que busca um valor específico na árvore.

Na classe `BinarySearchTree`, temos os métodos `insertValue:` que cria um novo nó com o valor informado e o insere na árvore, e `searchValue:` que busca um valor na árvore.

No método `main`, criamos uma instância de `BinarySearchTree` e inserimos alguns valores na árvore. Em seguida, realizamos uma busca pelo valor 6 e exibimos uma mensagem de acordo com o resultado.

Espero que este código atenda às suas necessidades!