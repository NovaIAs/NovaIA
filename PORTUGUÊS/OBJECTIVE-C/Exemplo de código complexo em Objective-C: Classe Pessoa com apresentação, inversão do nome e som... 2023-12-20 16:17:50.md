Claro! Aqui está um exemplo de um código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

// Classe de exemplo
@interface Pessoa : NSObject

@property(nonatomic, strong) NSString *nome;
@property(nonatomic, assign) int idade;

- (instancetype)initWithNome:(NSString *)nome andIdade:(int)idade;
- (void)apresentar;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome andIdade:(int)idade {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
    }
    return self;
}

- (void)apresentar {
    NSLog(@"Oi, meu nome é %@ e tenho %d anos.", self.nome, self.idade);
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Criando uma instância de Pessoa
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" andIdade:25];
        
        // Apresentando a pessoa
        [pessoa apresentar];
        
        // Invertendo o nome da pessoa
        NSString *nomeInvertido = [[NSString alloc] init];
        for (NSInteger i = pessoa.nome.length - 1; i >= 0; i--) {
            nomeInvertido = [nomeInvertido stringByAppendingString:[NSString stringWithFormat:@"%C", [pessoa.nome characterAtIndex:i]]];
        }
        
        NSLog(@"Meu nome invertido é: %@", nomeInvertido);
        
        // Calculando a soma dos dígitos da idade
        int somaDigitos = 0;
        int idade = pessoa.idade;
        while (idade != 0) {
            somaDigitos += idade % 10;
            idade /= 10;
        }
        
        NSLog(@"A soma dos dígitos da minha idade é: %d", somaDigitos);
    }
    return 0;
}
```

Neste código, temos a definição de uma classe chamada `Pessoa`, que tem propriedades para nome e idade, um inicializador personalizado e um método de apresentação. Em seguida, temos a função principal `main`, onde criamos uma instância de `Pessoa`, apresentamos a pessoa, invertamos seu nome e calculamos a soma dos dígitos da idade.

Espero que este código atenda às suas expectativas!