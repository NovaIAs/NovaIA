Claro! Abaixo está um código em Objective-C que simula um sistema de gerenciamento de produtos em uma loja. O código possui comentários explicativos em cada seção para facilitar a compreensão.

```objective-c
// Importar bibliotecas
#import <Foundation/Foundation.h>

// Definir a classe Produto
@interface Produto : NSObject

@property NSString *nome;
@property float preco;

@end

@implementation Produto

@end

// Definir a classe Loja
@interface Loja : NSObject

@property NSMutableArray *produtos;

// Métodos
- (void)adicionarProduto:(Produto *)produto;
- (void)removerProduto:(NSString *)nome;
- (void)listarProdutos;
- (float)calcularTotal;

@end

@implementation Loja

- (instancetype)init {
    self = [super init];
    if (self) {
        self.produtos = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)adicionarProduto:(Produto *)produto {
    [self.produtos addObject:produto];
    NSLog(@"Produto \"%@\" adicionado com sucesso.", produto.nome);
}

- (void)removerProduto:(NSString *)nome {
    Produto *produtoRemover;
    for (Produto *produto in self.produtos) {
        if ([produto.nome isEqualToString:nome]) {
            produtoRemover = produto;
            break;
        }
    }
    if (produtoRemover) {
        [self.produtos removeObject:produtoRemover];
        NSLog(@"Produto \"%@\" removido com sucesso.", nome);
    } else {
        NSLog(@"Produto \"%@\" não encontrado.", nome);
    }
}

- (void)listarProdutos {
    NSLog(@"Lista de Produtos:");
    for (Produto *produto in self.produtos) {
        NSLog(@"- %@ (R$ %.2f)", produto.nome, produto.preco);
    }
}

- (float)calcularTotal {
    float total = 0.0;
    for (Produto *produto in self.produtos) {
        total += produto.preco;
    }
    return total;
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criar instância da loja
        Loja *minhaLoja = [[Loja alloc] init];
        
        // Criar produtos
        Produto *produto1 = [[Produto alloc] init];
        produto1.nome = @"Camiseta";
        produto1.preco = 29.90;
        
        Produto *produto2 = [[Produto alloc] init];
        produto2.nome = @"Calça Jeans";
        produto2.preco = 99.90;
        
        Produto *produto3 = [[Produto alloc] init];
        produto3.nome = @"Tênis";
        produto3.preco = 149.90;
        
        // Adicionar produtos à loja
        [minhaLoja adicionarProduto:produto1];
        [minhaLoja adicionarProduto:produto2];
        [minhaLoja adicionarProduto:produto3];
        
        // Listar produtos
        [minhaLoja listarProdutos];
        
        // Calcular total
        float total = [minhaLoja calcularTotal];
        NSLog(@"Total: R$ %.2f", total);
        
        // Remover um produto
        [minhaLoja removerProduto:@"Calça Jeans"];
        
        // Listar produtos novamente
        [minhaLoja listarProdutos];
        
        // Calcular total novamente
        total = [minhaLoja calcularTotal];
        NSLog(@"Total: R$ %.2f", total);
    }
    return 0;
}
```

Este código simula um sistema de gerenciamento de produtos em uma loja. A classe `Produto` representa um produto com um nome e um preço. A classe `Loja` é responsável por gerenciar uma lista de produtos, permitindo adicionar, remover, listar e calcular o total dos produtos.

No método `main`, são criados três produtos e adicionados à loja. Em seguida, a lista de produtos é exibida e o total é calculado. Depois, um produto é removido, a lista é exibida novamente e o total é recalculado. Os resultados são exibidos no console.

Espero que este código atenda às suas necessidades!