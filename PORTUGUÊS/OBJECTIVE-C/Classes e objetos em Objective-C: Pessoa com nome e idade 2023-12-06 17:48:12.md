Claro, vou gerar um código complexo em OBJECTIVE-C para você. Aqui está:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject {
    NSString *_nome;
    int _idade;
}

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) int idade;

- (instancetype)initWithNome:(NSString *)nome andIdade:(int)idade;
- (void)apresentar;
- (void)fazerAniversario;

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
    NSLog(@"Olá, meu nome é %@ e tenho %d anos.", _nome, _idade);
}

- (void)fazerAniversario {
    _idade++;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *p1 = [[Pessoa alloc] initWithNome:@"João" andIdade:25];
        Pessoa *p2 = [[Pessoa alloc] initWithNome:@"Maria" andIdade:30];
        
        [p1 apresentar];
        [p1 fazerAniversario];
        [p1 apresentar];
        
        [p2 apresentar];
        [p2 fazerAniversario];
        [p2 apresentar];
    }
    return 0;
}
```

Neste código, criamos uma classe `Pessoa` que representa uma pessoa com nome e idade. Temos dois atributos privados `_nome` e `_idade` e dois getters/setters para esses atributos (`nome` e `idade`).

Na implementação da classe `Pessoa`, temos o método `initWithNome:andIdade:` que é utilizado para inicializar a instância com um nome e idade específicos. Também temos os métodos `apresentar` e `fazerAniversario`. O método `apresentar` imprime na saída a informação do nome e idade da pessoa, enquanto o método `fazerAniversario` incrementa a idade em 1.

No `main`, criamos duas instâncias da classe `Pessoa`, uma chamada `p1` com nome "João" e idade 25, e outra chamada `p2` com nome "Maria" e idade 30. Em seguida, chamamos o método `apresentar` para exibir as informações iniciais de cada pessoa, fazemos um aniversário para cada pessoa usando o método `fazerAniversario` e chamamos novamente o método `apresentar` para exibir as informações atualizadas após o aniversário ter sido feito.

Espero que este exemplo possa atender às suas expectativas. Se precisar de mais informações ou de auxílio adicional, fique à vontade para perguntar.