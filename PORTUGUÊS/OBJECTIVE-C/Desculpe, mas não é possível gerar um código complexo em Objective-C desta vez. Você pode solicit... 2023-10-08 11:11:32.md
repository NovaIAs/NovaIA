Claro! Aqui está um exemplo de um código complexo em Objective-C que envolve a criação de uma classe de árvore binária de busca:

```objective-c
// Definindo a estrutura de um nó da árvore binária
@interface TreeNode : NSObject
@property (nonatomic, strong) id value;
@property (nonatomic, strong) TreeNode *left;
@property (nonatomic, strong) TreeNode *right;
@end

@implementation TreeNode
@end

// Classe da árvore binária de busca
@interface BinarySearchTree : NSObject
@property (nonatomic, strong) TreeNode *root;

- (void)insertValue:(id)value;
- (BOOL)containsValue:(id)value;
- (void)removeValue:(id)value;
@end

@implementation BinarySearchTree

// Método para inserir um valor na árvore
- (void)insertValue:(id)value {
    TreeNode *newNode = [[TreeNode alloc] init];
    newNode.value = value;
    
    if (self.root == nil) {
        self.root = newNode;
    } else {
        [self insertNode:newNode intoSubtree:self.root];
    }
}

// Método auxiliar para inserir um nó na subárvore
- (void)insertNode:(TreeNode *)newNode intoSubtree:(TreeNode *)subtree {
    if ([newNode.value isLessThan:subtree.value]) {
        if (subtree.left == nil) {
            subtree.left = newNode;
        } else {
            [self insertNode:newNode intoSubtree:subtree.left];
        }
    } else {
        if (subtree.right == nil) {
            subtree.right = newNode;
        } else {
            [self insertNode:newNode intoSubtree:subtree.right];
        }
    }
}

// Método para verificar se um valor está presente na árvore
- (BOOL)containsValue:(id)value {
    return [self searchNodeWithValue:value inSubtree:self.root] != nil;
}

// Método auxiliar para buscar um nó com um determinado valor na subárvore
- (TreeNode *)searchNodeWithValue:(id)value inSubtree:(TreeNode *)subtree {
    if (subtree == nil || [subtree.value isEqual:value]) {
        return subtree;
    }
    
    if ([value isLessThan:subtree.value]) {
        return [self searchNodeWithValue:value inSubtree:subtree.left];
    } else {
        return [self searchNodeWithValue:value inSubtree:subtree.right];
    }
}

// Método para remover um valor da árvore
- (void)removeValue:(id)value {
    self.root = [self removeNodeWithValue:value fromSubtree:self.root];
}

// Método auxiliar para remover um nó com um determinado valor da subárvore
- (TreeNode *)removeNodeWithValue:(id)value fromSubtree:(TreeNode *)subtree {
    if (subtree == nil) {
        return nil;
    }
    
    if ([value isLessThan:subtree.value]) {
        subtree.left = [self removeNodeWithValue:value fromSubtree:subtree.left];
    } else if ([value isGreaterThan:subtree.value]) {
        subtree.right = [self removeNodeWithValue:value fromSubtree:subtree.right];
    } else {
        if (subtree.left == nil) {
            return subtree.right;
        } else if (subtree.right == nil) {
            return subtree.left;
        } else {
            TreeNode *successor = [self findMinNodeInSubtree:subtree.right];
            subtree.value = successor.value;
            subtree.right = [self removeNodeWithValue:successor.value fromSubtree:subtree.right];
        }
    }
    
    return subtree;
}

// Método auxiliar para encontrar o nó com o menor valor em uma subárvore
- (TreeNode *)findMinNodeInSubtree:(TreeNode *)subtree {
    while (subtree.left != nil) {
        subtree = subtree.left;
    }
    
    return subtree;
}

@end

// Exemplo de uso da classe BinarySearchTree
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        BinarySearchTree *tree = [[BinarySearchTree alloc] init];
        
        [tree insertValue:@5];
        [tree insertValue:@2];
        [tree insertValue:@8];
        [tree insertValue:@1];
        [tree insertValue:@3];
        
        NSLog(@"A árvore contém o valor 3? %@", [tree containsValue:@3] ? @"Sim" : @"Não");
        
        [tree removeValue:@2];
        
        NSLog(@"A árvore contém o valor 2? %@", [tree containsValue:@2] ? @"Sim" : @"Não");
    }
    return 0;
}
```

Neste exemplo, criamos a classe `TreeNode` para representar um nó da árvore binária de busca, com propriedades `value`, `left` e `right`. Em seguida, criamos a classe `BinarySearchTree` para representar a própria árvore, com uma propriedade `root` e métodos para inserir, buscar e remover valores.

O método `insertValue:` insere um novo valor na árvore, criando um novo nó e percorrendo a árvore até encontrar o local adequado para inserção. O método `containsValue:` verifica se um determinado valor está presente na árvore, percorrendo a árvore em busca do valor desejado. O método `removeValue:` remove um valor da árvore, buscando o nó com o valor desejado e realizando as operações necessárias de remoção.

No exemplo de uso, criamos uma instância da classe `BinarySearchTree`, inserimos alguns valores na árvore e verificamos se a árvore contém determinados valores. Em seguida, removemos um valor da árvore e verificamos novamente se o valor foi removido com sucesso.

Espero que este exemplo tenha sido útil para você!