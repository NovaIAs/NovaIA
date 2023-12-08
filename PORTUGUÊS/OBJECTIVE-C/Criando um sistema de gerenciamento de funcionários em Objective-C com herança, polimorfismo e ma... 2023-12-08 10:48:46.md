Claro! Aqui está um código complexo em Objective-C que envolve a utilização de herança, polimorfismo e manipulação de coleções. O código simula a criação de um sistema de gerenciamento de funcionários de uma empresa:

```objective-c
#import <Foundation/Foundation.h>

// Classe base Funcionario
@interface Funcionario : NSObject

@property NSString *nome;
@property int idade;
@property int salario;

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(int)salario;
- (void)imprimirInformacoes;

@end

@implementation Funcionario

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(int)salario {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
        _salario = salario;
    }
    return self;
}

- (void)imprimirInformacoes {
    NSLog(@"Nome: %@, Idade: %d, Salário: R$%d", self.nome, self.idade, self.salario);
}

@end

// Classe derivada Gerente
@interface Gerente : Funcionario

@property int numFuncionarios;

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(int)salario eNumFuncionarios:(int)numFuncionarios;
- (void)aumentarSalario;
- (void)imprimirInformacoes;

@end

@implementation Gerente

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(int)salario eNumFuncionarios:(int)numFuncionarios {
    self = [super initWithNome:nome eIdade:idade eSalario:salario];
    if (self) {
        _numFuncionarios = numFuncionarios;
    }
    return self;
}

- (void)aumentarSalario {
    if (self.numFuncionarios > 10) {
        int bonus = self.numFuncionarios / 10;
        self.salario += 500 * bonus;
    }
}

- (void)imprimirInformacoes {
    NSLog(@"Nome: %@, Idade: %d, Salário: R$%d, Número de Funcionários: %d", self.nome, self.idade, self.salario, self.numFuncionarios);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criação de objetos
        Funcionario *func1 = [[Funcionario alloc] initWithNome:@"João" eIdade:30 eSalario:3000];
        Funcionario *func2 = [[Funcionario alloc] initWithNome:@"Maria" eIdade:25 eSalario:2500];
        Gerente *gerente1 = [[Gerente alloc] initWithNome:@"Carlos" eIdade:40 eSalario:5000 eNumFuncionarios:12];
        Gerente *gerente2 = [[Gerente alloc] initWithNome:@"Ana" eIdade:35 eSalario:4000 eNumFuncionarios:7];
        
        // Criação de uma coleção para armazenar os funcionários
        NSMutableArray *funcionarios = [[NSMutableArray alloc] init];
        [funcionarios addObject:func1];
        [funcionarios addObject:func2];
        [funcionarios addObject:gerente1];
        [funcionarios addObject:gerente2];
        
        // Impressão das informações de todos os funcionários
        for (Funcionario *funcionario in funcionarios) {
            [funcionario imprimirInformacoes];
        }
        
        // Aumento de salário para os gerentes
        [gerente1 aumentarSalario];
        [gerente2 aumentarSalario];
        
        NSLog(@"--- Após o aumento de salário ---");
        
        // Impressão das informações atualizadas dos gerentes
        [gerente1 imprimirInformacoes];
        [gerente2 imprimirInformacoes];
    }
    return 0;
}
```

Este código cria uma classe base chamada `Funcionario` que contém as propriedades `nome`, `idade` e `salario`, bem como um método `imprimirInformacoes` para exibir as informações do funcionário.

Temos também uma subclasse chamada `Gerente`, que herda da classe `Funcionario` e adiciona a propriedade `numFuncionarios` e os métodos `aumentarSalario` (que aumenta o salário com base no número de funcionários) e `imprimirInformacoes` (que imprime as informações específicas do gerente).

O programa principal cria objetos `Funcionario` e `Gerente`, armazena-os em uma coleção `NSMutableArray` e imprime as informações de cada funcionário. Em seguida, aumenta o salário dos gerentes e imprime suas informações atualizadas.

Espero que este exemplo atenda às suas expectativas! Sinta-se à vontade para fazer modificações ou pedir mais detalhes sobre o código gerado.