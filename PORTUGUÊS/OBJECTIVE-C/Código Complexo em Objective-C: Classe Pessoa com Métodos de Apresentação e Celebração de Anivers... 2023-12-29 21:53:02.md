Claro! Aqui está um exemplo de código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome eIdade:(NSInteger)idade;
- (void)apresentar;
- (void)celebrarAniversario;

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

- (void)apresentar {
    NSLog(@"Olá, meu nome é %@ e tenho %ld anos.", self.nome, (long)self.idade);
}

- (void)celebrarAniversario {
    self.idade++;
    NSLog(@"Feliz aniversário! Agora tenho %ld anos.", (long)self.idade);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" eIdade:25];
        [pessoa apresentar];
        [pessoa celebrarAniversario];
        [pessoa apresentar];
    }
    return 0;
}
```

Neste código, temos uma classe chamada "Pessoa" que representa uma pessoa com atributos de nome e idade. A classe possui um método de inicialização que recebe o nome e a idade da pessoa, além de métodos para apresentar a pessoa e celebrar o aniversário, incrementando a idade em 1.

No método `main`, um objeto da classe `Pessoa` é criado com nome "João" e idade 25, e em seguida, chamamos o método `apresentar` para exibir as informações da pessoa. Depois, chamamos o método `celebrarAniversario` para incrementar a idade em 1 e exibimos novamente as informações atualizadas.

Espero que este exemplo tenha atendido às suas expectativas!