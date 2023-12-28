Claro! Aqui está um código complexo em Objective-C que é bastante diferenciado. Este código é um exemplo de um sistema de gerenciamento de funcionários para uma empresa fictícia chamada "EmpresaXYZ". Ele utiliza classes, métodos e propriedades para armazenar e manipular informações sobre os funcionários.

```objective-c
// Classe Funcionario.h

#import <Foundation/Foundation.h>

@interface Funcionario : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSString *cargo;
@property (nonatomic, strong) NSNumber *salario;

- (instancetype)initWithNome:(NSString *)nome cargo:(NSString *)cargo salario:(NSNumber *)salario;
- (void)imprimirInformacoes;

@end


// Classe Funcionario.m

#import "Funcionario.h"

@implementation Funcionario

- (instancetype)initWithNome:(NSString *)nome cargo:(NSString *)cargo salario:(NSNumber *)salario {
    self = [super init];
    if (self) {
        _nome = nome;
        _cargo = cargo;
        _salario = salario;
    }
    return self;
}

- (void)imprimirInformacoes {
    NSLog(@"Nome: %@", self.nome);
    NSLog(@"Cargo: %@", self.cargo);
    NSLog(@"Salario: R$%.2f", self.salario.floatValue);
}

@end


// Classe GerenciamentoFuncionarios.h

#import <Foundation/Foundation.h>

@interface GerenciamentoFuncionarios : NSObject

@property (nonatomic, strong) NSMutableArray *funcionarios;

- (void)adicionarFuncionario:(Funcionario *)funcionario;
- (void)removerFuncionario:(Funcionario *)funcionario;
- (void)imprimirListaFuncionarios;
- (NSArray *)buscarFuncionariosPorCargo:(NSString *)cargo;

@end


// Classe GerenciamentoFuncionarios.m

#import "GerenciamentoFuncionarios.h"

@implementation GerenciamentoFuncionarios

- (instancetype)init {
    self = [super init];
    if (self) {
        _funcionarios = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)adicionarFuncionario:(Funcionario *)funcionario {
    [self.funcionarios addObject:funcionario];
}

- (void)removerFuncionario:(Funcionario *)funcionario {
    [self.funcionarios removeObject:funcionario];
}

- (void)imprimirListaFuncionarios {
    for (Funcionario *funcionario in self.funcionarios) {
        [funcionario imprimirInformacoes];
        NSLog(@"------------------------------");
    }
}

- (NSArray *)buscarFuncionariosPorCargo:(NSString *)cargo {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"cargo == %@", cargo];
    return [self.funcionarios filteredArrayUsingPredicate:predicate];
}

@end


// Classe Principal

#import <Foundation/Foundation.h>
#import "Funcionario.h"
#import "GerenciamentoFuncionarios.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        GerenciamentoFuncionarios *gerenciamento = [[GerenciamentoFuncionarios alloc] init];
        
        // Criar alguns funcionários
        Funcionario *funcionario1 = [[Funcionario alloc] initWithNome:@"João" cargo:@"Desenvolvedor" salario:@4500.00];
        Funcionario *funcionario2 = [[Funcionario alloc] initWithNome:@"Maria" cargo:@"Designer" salario:@3500.00];
        Funcionario *funcionario3 = [[Funcionario alloc] initWithNome:@"Pedro" cargo:@"Gerente de Projetos" salario:@6000.00];
        
        // Adicionar funcionários ao sistema
        [gerenciamento adicionarFuncionario:funcionario1];
        [gerenciamento adicionarFuncionario:funcionario2];
        [gerenciamento adicionarFuncionario:funcionario3];
        
        // Imprimir a lista de funcionários
        [gerenciamento imprimirListaFuncionarios];
        
        // Remover um funcionário
        [gerenciamento removerFuncionario:funcionario2];
        
        // Imprimir a lista atualizada de funcionários
        [gerenciamento imprimirListaFuncionarios];
        
        // Buscar funcionários por cargo
        NSArray *desenvolvedores = [gerenciamento buscarFuncionariosPorCargo:@"Desenvolvedor"];
        for (Funcionario *funcionario in desenvolvedores) {
            [funcionario imprimirInformacoes];
            NSLog(@"------------------------------");
        }
    }
    return 0;
}
```

Neste exemplo, temos as classes `Funcionario` e `GerenciamentoFuncionarios`. A classe `Funcionario` representa os dados de um funcionário, como nome, cargo e salário. A classe `GerenciamentoFuncionarios` é responsável por gerenciar uma lista de funcionários, permitindo adicionar, remover, imprimir a lista e buscar funcionários por cargo.

No programa principal, criamos uma instância do objeto `GerenciamentoFuncionarios` e então criamos alguns objetos `Funcionario` e os adicionamos ao sistema. Em seguida, imprimimos a lista de funcionários, removemos um funcionário e imprimimos a lista atualizada. Por fim, buscamos funcionários com o cargo de "Desenvolvedor" e imprimimos suas informações.

Espero que este código complexo em Objective-C possa ajudar a entender melhor o funcionamento da linguagem e a manipulação de classes e objetos.