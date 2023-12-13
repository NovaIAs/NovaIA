Claro! Aqui está um exemplo de um código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome eIdade:(NSInteger)idade;
- (void)apresentar;

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
    NSLog(@"Olá! Meu nome é %@ e tenho %ld anos.", self.nome, (long)self.idade);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" eIdade:25];
        [pessoa apresentar];
    }
    return 0;
}
```

Neste código, temos uma classe chamada "Pessoa" que possui duas propriedades: "nome" e "idade". Além disso, a classe possui um inicializador personalizado initWithNome:eIdade: e um método chamado "apresentar" que imprime na console uma mensagem com o nome e idade da pessoa.

No método principal (main), criamos uma instância da classe Pessoa chamada "pessoa" com o nome "João" e idade 25, e em seguida chamamos o método "apresentar" para exibir a mensagem na console.

Espero que este exemplo atenda às suas expectativas!