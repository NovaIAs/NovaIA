Claro! Aqui está um código complexo em Objective-C:

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
        self.nome = nome;
        self.idade = idade;
    }
    return self;
}

- (void)apresentar {
    NSLog(@"Olá, meu nome é %@ e eu tenho %ld anos!", self.nome, (long)self.idade);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        Pessoa *pessoa1 = [[Pessoa alloc] initWithNome:@"João" eIdade:25];
        [pessoa1 apresentar];
        
        Pessoa *pessoa2 = [[Pessoa alloc] initWithNome:@"Maria" eIdade:30];
        [pessoa2 apresentar];
        
        return 0;
    }
}
```

Neste código em Objective-C, criamos uma classe chamada `Pessoa` que representa uma pessoa com nome e idade. A classe possui uma inicialização customizada e um método `apresentar` para mostrar as informações da pessoa no console.

No trecho `main`, criamos duas instâncias da classe `Pessoa`, cada uma com um nome e idade diferentes, e chamamos o método `apresentar` para exibir as informações no console.

Esse código é apenas um exemplo de como usar classes e objetos em Objective-C. Você pode adaptá-lo e adicionar mais funcionalidades de acordo com suas necessidades.