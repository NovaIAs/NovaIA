Claro! Aqui está um código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property NSString *nome;
@property NSInteger idade;

- (void)apresentar;

@end

@implementation Pessoa

- (void)apresentar {
    NSLog(@"Olá, meu nome é %@ e eu tenho %ld anos.", self.nome, (long)self.idade);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        Pessoa *pessoa = [[Pessoa alloc] init];
        pessoa.nome = @"João";
        pessoa.idade = 25;
        
        [pessoa apresentar];
        
    }
    return 0;
}
```

Neste código, estamos criando uma classe chamada "Pessoa" que herda da classe NSObject. Essa classe possui duas propriedades: "nome", do tipo NSString, e "idade", do tipo NSInteger.

Dentro da implementação da classe Pessoa, temos um método chamado "apresentar". Esse método utiliza o NSLog para imprimir uma mensagem na console, exibindo o nome e a idade da pessoa.

No método principal "main", criamos uma instância da classe Pessoa chamada "pessoa". Em seguida, definimos o nome e a idade dessa pessoa.

Por fim, chamamos o método "apresentar" da instância "pessoa", que imprime a mensagem na console.

Espero que este código atenda às suas expectativas! Se precisar de mais ajuda, estou à disposição.