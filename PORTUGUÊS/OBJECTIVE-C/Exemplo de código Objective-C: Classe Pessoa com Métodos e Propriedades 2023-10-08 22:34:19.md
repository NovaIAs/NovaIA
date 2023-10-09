Claro! Aqui está um exemplo de um código complexo em Objective-C que envolve a criação de uma classe chamada "Pessoa" e a implementação de vários métodos e propriedades:

```objective-c
// Pessoa.h

#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property(nonatomic, strong) NSString *nome;
@property(nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade;
- (void)falar;
- (void)andar;
- (void)dormir;

@end


// Pessoa.m

#import "Pessoa.h"

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade {
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

- (void)dormir {
    NSLog(@"%@ está dormindo.", self.nome);
}

@end


// main.m

#import <Foundation/Foundation.h>
#import "Pessoa.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *p1 = [[Pessoa alloc] initWithNome:@"João" idade:25];
        Pessoa *p2 = [[Pessoa alloc] initWithNome:@"Maria" idade:30];
        
        [p1 falar];
        [p2 andar];
        [p1 dormir];
    }
    return 0;
}
```

Neste código, definimos uma classe chamada "Pessoa" que herda de NSObject. Essa classe possui duas propriedades: "nome" e "idade", ambas declaradas como strong e assign, respectivamente.

Em seguida, implementamos os métodos da classe. O método initWithNome:idade: é um inicializador personalizado que recebe o nome e a idade da pessoa e os atribui às propriedades correspondentes.

Os três métodos restantes, falar, andar e dormir, são métodos de instância que imprimem mensagens indicando que a pessoa está falando, andando ou dormindo, respectivamente. Esses métodos utilizam a propriedade "nome" para exibir o nome da pessoa.

No bloco main, criamos duas instâncias da classe Pessoa, passando diferentes nomes e idades para cada uma. Em seguida, chamamos os métodos falar, andar e dormir para cada uma das instâncias.

Ao executar este código, ele irá imprimir as seguintes mensagens no console:

```
João está falando.
Maria está andando.
João está dormindo.
```

Espero que este exemplo tenha sido útil para você entender melhor o uso de classes, propriedades e métodos em Objective-C.