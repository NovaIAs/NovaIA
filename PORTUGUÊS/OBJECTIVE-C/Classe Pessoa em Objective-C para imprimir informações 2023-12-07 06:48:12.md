Claro! Aqui está um código bastante complexo em Objective-C para você:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject
@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSUInteger idade;
@property (nonatomic, strong) NSDate *dataNascimento;

- (instancetype)initWithNome:(NSString *)nome idade:(NSUInteger)idade dataNascimento:(NSDate *)dataNascimento;
- (void)imprimirInformacoes;
@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome idade:(NSUInteger)idade dataNascimento:(NSDate *)dataNascimento {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
        _dataNascimento = dataNascimento;
    }
    return self;
}

- (void)imprimirInformacoes {
    NSDateFormatter *dateFormat = [[NSDateFormatter alloc] init];
    [dateFormat setDateFormat:@"dd/MM/yyyy"];
    NSString *dataNascimentoFormatada = [dateFormat stringFromDate:self.dataNascimento];

    NSLog(@"Nome: %@", self.nome);
    NSLog(@"Idade: %lu", (unsigned long)self.idade);
    NSLog(@"Data de Nascimento: %@", dataNascimentoFormatada);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Cria uma instância de Pessoa
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" idade:25 dataNascimento:[NSDate date]];
        [pessoa imprimirInformacoes];
    }
    return 0;
}
```

Neste código, temos a classe `Pessoa`, que representa uma pessoa com nome, idade e data de nascimento. Ela possui um método `initWithNome:idade:dataNascimento:` para inicializar os atributos, e um método `imprimirInformacoes` para imprimir os dados da pessoa no console.

No método `main`, criamos uma instância da classe `Pessoa` chamada `pessoa`, passando informações como nome "João", idade 25 e a data atual. Em seguida, chamamos o método `imprimirInformacoes` para exibir os dados da pessoa no console.

Espero que este exemplo atenda às suas expectativas e ajude a entender alguns conceitos de programação em Objective-C.