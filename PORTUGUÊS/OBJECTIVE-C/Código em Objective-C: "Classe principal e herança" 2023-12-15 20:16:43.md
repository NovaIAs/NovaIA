Claro, aqui está um código complexo em Objective-C para você:

```objective-c
#import <Foundation/Foundation.h>

// Classe principal
@interface MinhaClasse : NSObject

@property(nonatomic, strong) NSString *nome;
@property(nonatomic, assign) int idade;

- (void)apresentar;

@end

@implementation MinhaClasse

- (void)apresentar {
    NSLog(@"Olá, meu nome é %@ e eu tenho %d anos.", self.nome, self.idade);
}

@end

// Classe de exemplo para herança
@interface MinhaClasseFilha : MinhaClasse

@property(nonatomic, assign) int anoNascimento;

- (void)calcularIdade;

@end

@implementation MinhaClasseFilha

- (void)calcularIdade {
    int anoAtual = [[NSCalendar currentCalendar] component:NSCalendarUnitYear fromDate:[NSDate date]];
    self.idade = anoAtual - self.anoNascimento;
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MinhaClasse *objeto1 = [[MinhaClasse alloc] init];
        objeto1.nome = @"João";
        objeto1.idade = 25;
        [objeto1 apresentar];
        
        MinhaClasseFilha *objeto2 = [[MinhaClasseFilha alloc] init];
        objeto2.nome = @"Maria";
        objeto2.anoNascimento = 1988;
        [objeto2 calcularIdade];
        [objeto2 apresentar];
    }
    return 0;
}
```

Neste código, temos uma classe principal chamada "MinhaClasse" que possui duas propriedades: "nome" (uma string) e "idade" (um inteiro). A classe também possui um método de instância chamado "apresentar", que imprime uma mensagem na console com o nome e a idade do objeto.

Além disso, temos uma classe chamada "MinhaClasseFilha" que herda da classe "MinhaClasse" e adiciona uma nova propriedade chamada "anoNascimento" (um inteiro). A classe também possui um método chamado "calcularIdade" que calcula a idade atual com base no ano de nascimento e armazena o valor na propriedade "idade".

No método "main", criamos duas instâncias das classes "MinhaClasse" e "MinhaClasseFilha" e configuramos suas propriedades. Em seguida, chamamos os métodos "apresentar" e "calcularIdade" para exibir as informações na console.

Espero que esse código atenda às suas expectativas.