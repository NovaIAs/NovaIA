Claro, vou gerar um código complexo em Objective-C para você. Aqui está um exemplo de um código que cria uma classe chamada "Pessoa" com propriedades e métodos relacionados a informações pessoais. Vou explicar o código detalhadamente abaixo:

```objective-c
// Importando o framework Foundation
#import <Foundation/Foundation.h>

// Declarando a interface da classe Pessoa
@interface Pessoa : NSObject

// Propriedades da Pessoa
@property(nonatomic, strong) NSString *nome;
@property(nonatomic, assign) NSInteger idade;
@property(nonatomic, strong) NSString *endereco;

// Método de inicialização da Pessoa
- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade endereco:(NSString *)endereco;

// Método para exibir informações da Pessoa
- (void)exibirInformacoes;

@end

// Implementação da classe Pessoa
@implementation Pessoa

// Método de inicialização da Pessoa
- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade endereco:(NSString *)endereco {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
        _endereco = endereco;
    }
    return self;
}

// Método para exibir informações da Pessoa
- (void)exibirInformacoes {
    NSLog(@"Nome: %@", self.nome);
    NSLog(@"Idade: %ld", (long)self.idade);
    NSLog(@"Endereço: %@", self.endereco);
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criando uma instância da classe Pessoa
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" idade:25 endereco:@"Rua Principal, 123"];
        
        // Exibindo as informações da Pessoa
        [pessoa exibirInformacoes];
    }
    return 0;
}
```

Neste código, começamos importando o framework Foundation, que é necessário para utilizar as classes e métodos básicos do Objective-C. Em seguida, declaramos a interface da classe "Pessoa" com três propriedades: "nome", "idade" e "endereco".

Logo após, temos o método de inicialização da classe "Pessoa", que recebe os parâmetros "nome", "idade" e "endereco" e atribui esses valores às propriedades correspondentes da instância.

O método "exibirInformacoes" é responsável por exibir as informações da pessoa no console, utilizando a função "NSLog" para imprimir as mensagens formatadas.

Por fim, na função principal "main", criamos uma instância da classe "Pessoa" e chamamos o método "exibirInformacoes" para exibir as informações da pessoa criada.

Espero que este exemplo de código complexo em Objective-C atenda às suas expectativas!