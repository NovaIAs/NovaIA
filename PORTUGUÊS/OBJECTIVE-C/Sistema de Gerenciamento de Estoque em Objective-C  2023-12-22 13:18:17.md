Claro! Aqui está um exemplo de um código complexo em Objective-C, que implementa um sistema de gerenciamento de estoque para uma loja fictícia de roupas.

```objective-c
#import <Foundation/Foundation.h>

// Definição da classe Produto
@interface Produto : NSObject

@property NSString *codigo;
@property NSString *nome;
@property float preco;
@property int quantidade;

- (instancetype)initWithCodigo:(NSString *)codigo nome:(NSString *)nome preco:(float)preco quantidade:(int)quantidade;
- (void)adicionarQuantidade:(int)quantidade;
- (void)removerQuantidade:(int)quantidade;

@end

@implementation Produto

- (instancetype)initWithCodigo:(NSString *)codigo nome:(NSString *)nome preco:(float)preco quantidade:(int)quantidade {
    self = [super init];
    if (self) {
        _codigo = codigo;
        _nome = nome;
        _preco = preco;
        _quantidade = quantidade;
    }
    return self;
}

- (void)adicionarQuantidade:(int)quantidade {
    self.quantidade += quantidade;
}

- (void)removerQuantidade:(int)quantidade {
    if (self.quantidade >= quantidade) {
        self.quantidade -= quantidade;
    } else {
        NSLog(@"Não há quantidade suficiente em estoque");
    }
}

@end

// Definição da classe Loja
@interface Loja : NSObject

@property NSMutableDictionary *estoque;

- (void)adicionarProduto:(Produto *)produto;
- (void)removerProduto:(NSString *)codigo;
- (void)exibirEstoque;

@end

@implementation Loja

- (instancetype)init {
    self = [super init];
    if (self) {
        _estoque = [[NSMutableDictionary alloc] init];
    }
    return self;
}

- (void)adicionarProduto:(Produto *)produto {
    [self.estoque setObject:produto forKey:produto.codigo];
}

- (void)removerProduto:(NSString *)codigo {
    [self.estoque removeObjectForKey:codigo];
}

- (void)exibirEstoque {
    for (NSString *codigo in self.estoque) {
        Produto *produto = self.estoque[codigo];
        NSLog(@"Código: %@ | Nome: %@ | Preço: %.2f | Quantidade: %d", produto.codigo, produto.nome, produto.preco, produto.quantidade);
    }
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Loja *loja = [[Loja alloc] init];
        
        Produto *produto1 = [[Produto alloc] initWithCodigo:@"P001" nome:@"Camiseta" preco:29.90 quantidade:50];
        Produto *produto2 = [[Produto alloc] initWithCodigo:@"P002" nome:@"Calça Jeans" preco:99.90 quantidade:30];
        Produto *produto3 = [[Produto alloc] initWithCodigo:@"P003" nome:@"Tênis" preco:149.90 quantidade:20];
        
        [loja adicionarProduto:produto1];
        [loja adicionarProduto:produto2];
        [loja adicionarProduto:produto3];
        
        [loja exibirEstoque];
        
        [produto1 adicionarQuantidade:10];
        [produto2 removerQuantidade:5];
        
        [loja exibirEstoque];
    }
    return 0;
}
```

Neste código, temos a definição da classe `Produto`, que representa um produto da loja, e a classe `Loja`, que é responsável pelo gerenciamento do estoque. O código utiliza conceitos de orientação a objetos, como encapsulamento e herança.

No método principal `main`, são criados alguns produtos e adicionados à loja. Em seguida, é exibido o estoque inicial da loja. Depois, são realizadas algumas operações de adição e remoção de quantidade de produtos e, por fim, é exibido o estoque atualizado.

Espero que este código atenda às suas expectativas!