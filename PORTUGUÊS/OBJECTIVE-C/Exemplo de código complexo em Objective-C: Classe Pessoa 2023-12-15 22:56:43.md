Claro! Aqui está um exemplo de código complexo em Objective-C que utiliza várias funcionalidades e recursos da linguagem:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome andIdade:(NSInteger)idade;

- (void)falar;
- (void)andar;
- (void)correr;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome andIdade:(NSInteger)idade {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
    }
    return self;
}

- (void)falar {
    NSLog(@"%@ está falando.", self.nome);
}

- (void)andar {
    NSLog(@"%@ está andando.", self.nome);
}

- (void)correr {
    NSLog(@"%@ está correndo.", self.nome);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" andIdade:25];
        
        [pessoa falar];
        [pessoa andar];
        [pessoa correr];
        
        NSLog(@"%@ tem %ld anos.", pessoa.nome, (long)pessoa.idade);
    }
    return 0;
}
```

Neste exemplo, temos uma classe chamada "Pessoa" que representa uma pessoa com nome e idade. A classe possui um construtor que recebe o nome e a idade como parâmetros, além de métodos para falar, andar e correr. O método main cria uma instância da classe Pessoa, realiza chamadas aos métodos e imprime o nome e idade da pessoa.

Este código demonstra o uso de classes, propriedades, construtores, métodos e a estrutura básica de um programa Objective-C.