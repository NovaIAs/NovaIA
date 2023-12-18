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
    NSLog(@"Olá, meu nome é %@ e tenho %ld anos.", self.nome, (long)self.idade);
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

Neste exemplo, criamos uma classe chamada "Pessoa" que possui duas propriedades: "nome" e "idade". Também implementamos dois métodos: "initWithNome:eIdade:" para inicializar a classe com valores para as propriedades e "apresentar" para exibir uma mensagem com o nome e a idade da pessoa.

No método principal "main", criamos uma instância da classe "Pessoa" chamada "pessoa" com o nome "João" e a idade 25, em seguida chamamos o método "apresentar" para exibir a mensagem na saída do console.

Espero que este exemplo atenda às suas expectativas! Se tiver alguma dúvida, estou à disposição.