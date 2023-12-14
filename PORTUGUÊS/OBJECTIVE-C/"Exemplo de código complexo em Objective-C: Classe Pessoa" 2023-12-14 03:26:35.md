Claro! Aqui está um exemplo de um código complexo em Objective-C:

```
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome eIdade:(NSInteger)idade;
- (void)falar;

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

- (void)falar {
    NSLog(@"Olá, meu nome é %@ e eu tenho %ld anos.", self.nome, self.idade);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" eIdade:25];
        [pessoa falar];
    }
    return 0;
}
```

Neste código em Objective-C, criamos uma classe chamada "Pessoa" que possui duas propriedades: "nome" e "idade". A classe tem um inicializador personalizado que recebe um nome e uma idade como parâmetros. Também possui um método chamado "falar" que imprime no console uma mensagem com o nome e a idade da pessoa.

No bloco "main", criamos uma instância da classe "Pessoa" chamada "pessoa" com o nome "João" e a idade "25". Em seguida, chamamos o método "falar" para exibir a mensagem no console.

Espero que este código atenda às suas expectativas!