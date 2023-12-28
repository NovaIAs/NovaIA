Claro! Aqui está um exemplo de código complexo em Objective-C, com comentários explicativos em português:

```objective-c
#import <Foundation/Foundation.h>

// Declaração da classe Pessoa
@interface Pessoa : NSObject

// Propriedades da classe Pessoa
@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSString *sobrenome;
@property (nonatomic, assign) NSInteger idade;

// Método para imprimir os detalhes da pessoa
- (void)imprimirDetalhes;

@end

@implementation Pessoa

// Implementação do método imprimirDetalhes
- (void)imprimirDetalhes {
    NSLog(@"Nome: %@ %@", self.nome, self.sobrenome);
    NSLog(@"Idade: %ld", (long)self.idade);
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Instância da classe Pessoa
        Pessoa *pessoa = [[Pessoa alloc] init];
        
        // Atribuição dos valores às propriedades
        pessoa.nome = @"João";
        pessoa.sobrenome = @"Silva";
        pessoa.idade = 30;
        
        // Chamada do método imprimirDetalhes
        [pessoa imprimirDetalhes];
    }
    return 0;
}
```

Neste código, criamos uma classe chamada "Pessoa" que possui três propriedades: "nome", "sobrenome" e "idade". A classe também possui um método chamado "imprimirDetalhes", que imprime na saída padrão o nome completo e a idade da pessoa.

Na função principal "main", criamos uma instância da classe "Pessoa" chamada "pessoa" e atribuímos valores às suas propriedades. Em seguida, chamamos o método "imprimirDetalhes" para exibir os detalhes da pessoa na saída do programa.

Esse código é apenas um exemplo básico para demonstrar a estrutura de uma classe em Objective-C e como criar objetos a partir dessa classe. Você pode expandi-lo e adicionar mais funcionalidades de acordo com suas necessidades.