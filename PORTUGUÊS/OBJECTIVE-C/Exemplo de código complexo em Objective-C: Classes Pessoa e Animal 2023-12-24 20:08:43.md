Claro! Aqui está um exemplo de código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

// Classe para representar uma pessoa
@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome eIdade:(NSInteger)idade;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome eIdade:(NSInteger)idade {
    self = [super init];
    
    if (self) {
        _nome = nome;
        _idade = idade;
    }
    
    return self;
}

@end

// Classe para representar um animal
@interface Animal : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSString *especie;

- (instancetype)initWithNome:(NSString *)nome eEspecie:(NSString *)especie;
- (void)fazerBarulho;

@end

@implementation Animal

- (instancetype)initWithNome:(NSString *)nome eEspecie:(NSString *)especie {
    self = [super init];
    
    if (self) {
        _nome = nome;
        _especie = especie;
    }
    
    return self;
}

- (void)fazerBarulho {
    NSLog(@"%@ fazendo barulho!", self.nome);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criando uma pessoa
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" eIdade:30];
        NSLog(@"Nome: %@, Idade: %ld", pessoa.nome, (long)pessoa.idade);
        
        // Criando um animal
        Animal *animal = [[Animal alloc] initWithNome:@"Rex" eEspecie:@"Cachorro"];
        NSLog(@"Nome: %@, Espécie: %@", animal.nome, animal.especie);
        
        // Fazendo o animal fazer barulho
        [animal fazerBarulho];
    }
    
    return 0;
}
```

Neste código em Objective-C, temos duas classes: `Pessoa` e `Animal`. A classe `Pessoa` é responsável por representar uma pessoa, com propriedades como nome e idade. Ela possui um inicializador personalizado para definir essas propriedades.

A classe `Animal`, por sua vez, representa um animal, com propriedades como nome e espécie. Ela também possui um inicializador personalizado e um método `fazerBarulho` que imprime uma mensagem no console.

No `main`, criamos uma instância de `Pessoa` e uma instância de `Animal`, e exibimos as propriedades desses objetos no console. Em seguida, chamamos o método `fazerBarulho` do objeto `Animal`, que imprime a mensagem "Rex fazendo barulho!".

Este código ilustra alguns conceitos básicos de classes, propriedades, inicializadores e métodos em Objective-C. Espero que isso te ajude a entender melhor a linguagem!