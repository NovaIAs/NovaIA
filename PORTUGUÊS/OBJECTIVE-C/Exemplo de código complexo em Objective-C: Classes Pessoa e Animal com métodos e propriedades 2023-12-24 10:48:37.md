Claro! Aqui está um exemplo de um código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
    }
    return self;
}

@end

@interface Animal : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSString *tipo;

- (instancetype)initWithNome:(NSString *)nome tipo:(NSString *)tipo;

- (void)fazerBarulho;

@end

@implementation Animal

- (instancetype)initWithNome:(NSString *)nome tipo:(NSString *)tipo {
    self = [super init];
    if (self) {
        _nome = nome;
        _tipo = tipo;
    }
    return self;
}

- (void)fazerBarulho {
    NSString *barulho = @"";
    if ([_tipo isEqualToString:@"cachorro"]) {
        barulho = @"Au Au!";
    } else if ([_tipo isEqualToString:@"gato"]) {
        barulho = @"Miau!";
    } else if ([_tipo isEqualToString:@"passaro"]) {
        barulho = @"Có Có!";
    } else {
        barulho = @"Barulho desconhecido";
    }
    
    NSLog(@"%@", barulho);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" idade:30];
        NSLog(@"Nome da pessoa: %@, idade: %ld", pessoa.nome, (long)pessoa.idade);
        
        Animal *cachorro = [[Animal alloc] initWithNome:@"Rex" tipo:@"cachorro"];
        [cachorro fazerBarulho];
        
        Animal *gato = [[Animal alloc] initWithNome:@"Felix" tipo:@"gato"];
        [gato fazerBarulho];
        
        Animal *passaro = [[Animal alloc] initWithNome:@"Piu" tipo:@"passaro"];
        [passaro fazerBarulho];
    }
    return 0;
}
```

Neste código, temos duas classes: `Pessoa` e `Animal`. A classe `Pessoa` é uma classe simples com duas propriedades (`nome` e `idade`) e um método de inicialização personalizado. A classe `Animal` também tem duas propriedades (`nome` e `tipo`) e um método de inicialização personalizado. Além disso, a classe `Animal` possui um método `fazerBarulho` que imprime um barulho específico com base no tipo de animal.

No método `main`, são criados objetos das classes `Pessoa` e `Animal` e são chamados métodos para exibir seus dados e fazer barulhos, respectivamente.

Esse código é apenas um exemplo e pode ser adaptado e expandido de várias maneiras. Espero que isso lhe dê uma ideia de como trabalhar com classes e objetos em Objective-C.