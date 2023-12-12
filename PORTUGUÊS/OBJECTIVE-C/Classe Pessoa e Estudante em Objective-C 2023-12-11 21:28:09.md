Claro, vou gerar um código complexo em Objective-C para você. Aqui está:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property (nonatomic, copy) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome andIdade:(NSInteger)idade;
- (void)apresentar;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome andIdade:(NSInteger)idade {
    self = [super init];
    if (self) {
        _nome = [nome copy];
        _idade = idade;
    }
    return self;
}

- (void)apresentar {
    NSLog(@"Olá, meu nome é %@ e tenho %ld anos.", self.nome, (long)self.idade);
}

@end

@interface Estudante : Pessoa

@property (nonatomic, copy) NSString *curso;
@property (nonatomic, assign) NSInteger anoIngresso;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade curso:(NSString *)curso andAnoIngresso:(NSInteger)anoIngresso;
- (void)estudar;

@end

@implementation Estudante

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade curso:(NSString *)curso andAnoIngresso:(NSInteger)anoIngresso {
    self = [super initWithNome:nome andIdade:idade];
    if (self) {
        _curso = [curso copy];
        _anoIngresso = anoIngresso;
    }
    return self;
}

- (void)estudar {
    NSLog(@"Estou estudando %@.", self.curso);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" andIdade:25];
        [pessoa apresentar];
        
        Estudante *estudante = [[Estudante alloc] initWithNome:@"Maria" idade:20 curso:@"Engenharia" andAnoIngresso:2018];
        [estudante apresentar];
        [estudante estudar];
    }
    return 0;
}
```

Neste código, temos uma classe `Pessoa` que possui duas propriedades: `nome` (do tipo `NSString`) e `idade` (do tipo `NSInteger`). A classe também possui um método de inicialização `initWithNome:andIdade:` e um método `apresentar` para exibir uma mensagem de apresentação na saída padrão.

Em seguida, temos uma classe `Estudante` que herda da classe `Pessoa` e possui duas propriedades adicionais: `curso` (do tipo `NSString`) e `anoIngresso` (do tipo `NSInteger`). A classe também possui um método de inicialização `initWithNome:idade:curso:andAnoIngresso:` e um método `estudar` para exibir uma mensagem informando o curso que o estudante está estudando.

No método `main`, criamos uma instância de `Pessoa` chamada `pessoa` e uma instância de `Estudante` chamada `estudante`. Em seguida, chamamos os métodos `apresentar` e `estudar` para exibir as mensagens correspondentes.

Espero que esse código complexo em Objective-C atenda às suas expectativas. Se você tiver alguma dúvida sobre o código, é só me perguntar!