Claro! Aqui está um exemplo de um código complexo em Objective-C. Este código é um exemplo de um sistema de gerenciamento de estoque em uma loja fictícia. Ele permite que os funcionários adicionem, removam e atualizem os produtos no estoque, além de fornecer informações sobre os produtos disponíveis.

```objective-c
// Importando as bibliotecas necessárias
#import <Foundation/Foundation.h>

// Declaração da classe Produto
@interface Produto : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger quantidade;
@property (nonatomic, assign) float preco;

@end

@implementation Produto

@end

// Declaração da classe Loja
@interface Loja : NSObject

@property (nonatomic, strong) NSMutableArray *estoque;

// Métodos para gerenciar o estoque
- (void)adicionarProduto:(Produto *)produto;
- (void)removerProduto:(Produto *)produto;
- (void)atualizarPrecoDoProduto:(Produto *)produto novoPreco:(float)novoPreco;
- (void)exibirEstoque;

@end

@implementation Loja

// Inicialização do estoque
- (instancetype)init {
    self = [super init];
    if (self) {
        _estoque = [[NSMutableArray alloc] init];
    }
    return self;
}

// Adicionar produto ao estoque
- (void)adicionarProduto:(Produto *)produto {
    [self.estoque addObject:produto];
    NSLog(@"Produto %@ adicionado ao estoque.", produto.nome);
}

// Remover produto do estoque
- (void)removerProduto:(Produto *)produto {
    [self.estoque removeObject:produto];
    NSLog(@"Produto %@ removido do estoque.", produto.nome);
}

// Atualizar o preço de um produto no estoque
- (void)atualizarPrecoDoProduto:(Produto *)produto novoPreco:(float)novoPreco {
    produto.preco = novoPreco;
    NSLog(@"Preço do produto %@ atualizado para %.2f.", produto.nome, produto.preco);
}

// Exibir o estoque atual
- (void)exibirEstoque {
    NSLog(@"Estoque:");
    for (Produto *produto in self.estoque) {
        NSLog(@"Produto: %@ | Quantidade: %ld | Preço: %.2f", produto.nome, (long)produto.quantidade, produto.preco);
    }
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Criação da loja
        Loja *loja = [[Loja alloc] init];
        
        // Criação de produtos
        Produto *produto1 = [[Produto alloc] init];
        produto1.nome = @"Camiseta";
        produto1.quantidade = 10;
        produto1.preco = 29.99;
        
        Produto *produto2 = [[Produto alloc] init];
        produto2.nome = @"Calça Jeans";
        produto2.quantidade = 5;
        produto2.preco = 99.99;
        
        // Adicionar produtos ao estoque
        [loja adicionarProduto:produto1];
        [loja adicionarProduto:produto2];
        
        // Exibir estoque
        [loja exibirEstoque];
        
        // Remover produto do estoque
        [loja removerProduto:produto1];
        
        // Atualizar preço do produto
        [loja atualizarPrecoDoProduto:produto2 novoPreco:89.99];
        
        // Exibir estoque atualizado
        [loja exibirEstoque];
    }
    return 0;
}
```

Explicação do código:
1. Começamos importando a biblioteca Foundation, que fornece as funcionalidades básicas do Objective-C.
2. Em seguida, declaramos a classe "Produto" com as propriedades nome, quantidade e preço. Essa classe representa um produto em estoque.
3. Em seguida, declaramos a classe "Loja" com a propriedade "estoque" que é um array mutável que irá armazenar os produtos em estoque.
4. A classe "Loja" também possui métodos para adicionar, remover, atualizar e exibir o estoque de produtos.
5. A função "main" é a função principal do programa, onde criamos uma instância da classe "Loja" e alguns produtos fictícios.
6. Em seguida, utilizamos os métodos da classe "Loja" para adicionar os produtos ao estoque, exibir o estoque, remover um produto específico, atualizar o preço de outro produto e exibir o estoque atualizado.

Este código é apenas um exemplo de como poderia ser implementado um sistema de gerenciamento de estoque em Objective-C. É importante notar que existem muitas formas diferentes de implementar tal sistema, dependendo dos requisitos específicos do projeto.